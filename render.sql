CREATE TYPE render_shape AS (
    shape circle,
    color bytea
);

CREATE TYPE svgpath_step AS (
    svg_command char(1),
    args point[]
);

CREATE TYPE svgpath AS (
    steps svgpath_step[],
    width int,
    color bytea
);

CREATE OR REPLACE FUNCTION create_svgpath(d TEXT, width int, color bytea)
    RETURNS svgpath
    AS $$
    DECLARE
        x integer := 0;
        y integer := 0;
        z point := '(0,0)';
        q point := '(0,0)';
        i integer := 1;
        tokens text[];
        result svgpath_step[] := ARRAY[]::svgpath_step[];
    BEGIN
    tokens := regexp_split_to_array(trim(d), E'[,\\s]\\s*|(?<=[A-Za-z])');
    WHILE i <= array_length(tokens, 1) LOOP
        i := i + 1;
        CASE tokens[i - 1]
            WHEN 'M' THEN
                x := tokens[i + 0]::int;
                y := tokens[i + 1]::int;
                z := point (x, y);
                q := point (x, y);
                i := i + 2;
            WHEN 'm' THEN
                x := x + tokens[i + 0]::int;
                y := y + tokens[i + 1]::int;
                z := point (x, y);
                q := point (x, y);
                i := i + 2;
            WHEN 'Q' THEN
                WHILE i <= array_length(tokens, 1) AND tokens[i] !~* '[a-z]' LOOP
                    result := result || ('Q', ARRAY [
                        point(x, y),
                        point(tokens[i + 0]::int, tokens[i + 1]::int),
                        point(tokens[i + 2]::int, tokens[i + 3]::int)
                    ])::svgpath_step;
                    q := point (tokens[i + 2]::int * 2 - tokens[i + 0]::int, tokens[i + 3]::int * 2 - tokens[i + 1]::int);
                    x := tokens[i + 2]::int;
                    y := tokens[i + 3]::int;
                    i := i + 4;
                END LOOP;
            WHEN 'q' THEN
                WHILE i <= array_length(tokens, 1) AND tokens[i] !~* '[a-z]' LOOP
                    result := result || ('Q', ARRAY [
                        point(x, y),
                        point(x + tokens[i + 0]::int, y + tokens[i + 1]::int),
                        point(x + tokens[i + 2]::int, y + tokens[i + 3]::int)
                    ])::svgpath_step;
                    q := point (x + tokens[i + 2]::int * 2 - tokens[i + 0]::int, y + tokens[i + 3]::int * 2 - tokens[i + 1]::int);
                    x := x + tokens[i + 2]::int;
                    y := y + tokens[i + 3]::int;
                    i := i + 4;
                END LOOP;
            WHEN 'T' THEN
                WHILE i <= array_length(tokens, 1) AND tokens[i] !~* '[a-z]' LOOP
                    result := result || ('Q', ARRAY [
                        point(x, y),
                        q,
                        point(tokens[i + 0]::int, tokens[i + 1]::int)
                    ])::svgpath_step;
                    q := point (tokens[i + 0]::int * 2 - q[0]::int, tokens[i + 1]::int * 2 - q[1]::int);
                    x := tokens[i + 0]::int;
                    y := tokens[i + 1]::int;
                    i := i + 2;
                END LOOP;
            WHEN 't' THEN
                WHILE i <= array_length(tokens, 1) AND tokens[i] !~* '[a-z]' LOOP
                    result := result || ('Q', ARRAY [
                        point(x, y),
                        q,
                        point(x + tokens[i + 0]::int, y + tokens[i + 1]::int)
                    ])::svgpath_step;
                    q := point (x * 2 + tokens[i + 0]::int * 2 - q[0]::int, y * 2 + tokens[i + 1]::int * 2 - q[1]::int);
                    x := x + tokens[i + 0]::int;
                    y := y + tokens[i + 1]::int;
                    i := i + 2;
                END LOOP;
            WHEN 'Z' THEN
                result := result || ('L', ARRAY [point(x, y), z])::svgpath_step;
                x := z[0];
                y := z[1];
                q := point (x, y);
            WHEN 'z' THEN
                result := result || ('L', ARRAY [point(x, y), z])::svgpath_step;
                x := z[0];
                y := z[1];
                q := point (x, y);
            WHEN 'L' THEN
                WHILE i <= array_length(tokens, 1) AND tokens[i] !~* '[a-z]' LOOP
                    result := result || ('L', ARRAY [point(x, y), point(tokens[i + 0]::int, tokens[i + 1]::int)])::svgpath_step;
                    x := tokens[i + 0]::int;
                    y := tokens[i + 1]::int;
                    q := point (x, y);
                    i := i + 2;
                END LOOP;
            WHEN 'l' THEN
                WHILE i <= array_length(tokens, 1) AND tokens[i] !~* '[a-z]' LOOP
                    result := result || ('l', ARRAY [point(x, y), point(x + tokens[i + 0]::int, y + tokens[i + 1]::int)])::svgpath_step;
                    x := x + tokens[i + 0]::int;
                    y := y + tokens[i + 1]::int;
                    q := point (x, y);
                    i := i + 2;
                END LOOP;
            WHEN 'H' THEN
                WHILE i <= array_length(tokens, 1) AND tokens[i] !~* '[a-z]' LOOP
                    result := result || ('L', ARRAY [point(x, y), point(tokens[i]::int, y)])::svgpath_step;
                    x := tokens[i]::int;
                    q := point (x, y);
                    i := i + 1;
                END LOOP;
            WHEN 'h' THEN
                WHILE i <= array_length(tokens, 1) AND tokens[i] !~* '[a-z]' LOOP
                    result := result || ('L', ARRAY [point(x, y), point(x + tokens[i]::int, y)])::svgpath_step;
                    x := x + tokens[i]::int;
                    q := point (x, y);
                    i := i + 1;
                END LOOP;
            WHEN 'V' THEN
                WHILE i <= array_length(tokens, 1) AND tokens[i] !~* '[a-z]' LOOP
                    result := result || ('L', ARRAY [point(x, y), point(x, tokens[i]::int)])::svgpath_step;
                    y := tokens[i]::int;
                    q := point (x, y);
                    i := i + 1;
                END LOOP;
            WHEN 'v' THEN
                WHILE i <= array_length(tokens, 1) AND tokens[i] !~* '[a-z]' LOOP
                    result := result || ('L', ARRAY [point(x, y), point(x, y + tokens[i]::int)])::svgpath_step;
                    y := y + tokens[i]::int;
                    q := point (x, y);
                    i := i + 1;
                END LOOP;
            ELSE
                RAISE 'Unrecognized svgpath symbol % at token %', tokens[i], i;
        END CASE;
    END LOOP;
    RETURN (result, width, color)::svgpath;
    END;
$$ LANGUAGE plpgsql IMMUTABLE STRICT;

CREATE OR REPLACE FUNCTION left_of(p point, p0 point, p1 point)
    RETURNS bool
    AS $$
    BEGIN
        RETURN (p[0] - p0[0]) * (p1[1] - p0[1]) - (p[1] - p0[1]) * (p1[0] - p0[0]) < 0;
    END;
$$ LANGUAGE plpgsql IMMUTABLE STRICT;


CREATE OR REPLACE FUNCTION unit(point)
    RETURNS point
    AS $$
    DECLARE magnitude decimal;
    BEGIN
        magnitude := sqrt($1[0] * $1[0] + $1[1] * $1[1]);
        RETURN point ($1[0] / magnitude, $1[1] / magnitude);
    END;
$$ LANGUAGE plpgsql IMMUTABLE STRICT;


CREATE OR REPLACE FUNCTION expand_triangle(p0 point, p1 point, p2 point, amount decimal)
    RETURNS RECORD
    AS $$
    DECLARE
        leg0 point;
        leg1 point;
        leg2 point;
        ret RECORD;
    BEGIN
        leg0 := unit (p0 - p1) * point (amount, 0);
        leg1 := unit (p1 - p2) * point (amount, 0);
        leg2 := unit (p2 - p0) * point (amount, 0);
        SELECT
            p0 + leg0 - leg2,
            p1 + leg1 - leg0,
            p2 + leg2 - leg1
        INTO ret;
        RETURN ret;
    END;
$$ LANGUAGE plpgsql IMMUTABLE STRICT;
COMMENT ON FUNCTION expand_triangle(point, point, point, decimal) IS
    'Expand the edges of the triangle defined by the points out by `amount`.
     The expansion happens perpendicular away from the edges, meaning the vertices
     will expand more than `amount` away from the center.';


CREATE OR REPLACE FUNCTION dist2(point, point)
    RETURNS decimal
    AS $$
    BEGIN
        RETURN ($1[0] - $2[0]) * ($1[0] - $2[0]) + ($1[1] - $2[1]) * ($1[1] - $2[1]);
    END;
$$ LANGUAGE plpgsql IMMUTABLE STRICT;
COMMENT ON FUNCTION dist2(point, point) IS
    'Distance formula for two 2d points, but skipping the final sqrt.
     This can be a nice optimization when you just need to compare relative lengths';


CREATE OR REPLACE FUNCTION svgpath_contains(svgpath, point)
    RETURNS bool
    AS $$
    BEGIN
        RETURN EXISTS (
            SELECT 1
            FROM unnest(($1).steps) step
            WHERE (
                SELECT CASE (step).svg_command
                    WHEN 'Q' THEN
                        (
                            SELECT count(*) = 1 FROM (
                                SELECT 1
                                FROM expand_triangle((step).args[1], (step).args[2], (step).args[3], ($1).width)
                                AS (p0 point, p1 point, p2 point)
                                JOIN LATERAL (VALUES (p0, p1), (p1, p2), (p2, p0)) _(pa, pb) ON TRUE
                                GROUP BY left_of($2, pa, pb)
                            ) _
                        ) AND qbezier_distance($2, (step).args[1], (step).args[2], (step).args[3]) <= ($1).width
                    WHEN 'L' THEN $2 <-> lseg((step).args[1], (step).args[2]) <= ($1).width
                END
            )
        );
    END;
$$ LANGUAGE plpgsql IMMUTABLE STRICT;

CREATE OR REPLACE FUNCTION qbezier_distance(p point, p0 point, pc point, p1 point)
    RETURNS decimal
    AS $$
    DECLARE
        pos_x decimal;
        pos_y decimal;
        a_x decimal;
        a_y decimal;
        b_x decimal;
        b_y decimal;
        a decimal;
        b decimal;
        c decimal;
        d decimal;
        solutions decimal[];
        distMin decimal;
        d0 decimal;
        d1 decimal;
    BEGIN
        a_x := pc[0] - p0[0];
        a_y := pc[1] - p0[1];
        b_x := p0[0] - 2 * pc[0] + p1[0];
        b_y := p0[1] - 2 * pc[1] + p1[1];

        pos_x := p0[0] - p[0];
        pos_y := p0[1] - p[1];
        a := b_x * b_x + b_y * b_y;
        b := 3 * (a_x * b_x + a_y * b_y);
        c := 2 * (a_x * a_x + a_y * a_y) + pos_x * b_x + pos_y * b_y;
        d := pos_x * a_x + pos_y * a_y;
        solutions := third_degree_equation(a, b, c, d);

        CASE WHEN solutions IS NOT NULL THEN
            distMin := (
                SELECT min(pt <-> p)
                FROM (
                    SELECT bezier_point(t, p0, pc, p1) as pt FROM unnest(solutions) _(t)
                    WHERE t >= 0 AND t <= 1
                ) _(pt)
            );
        ELSE NULL;
        END CASE;

        CASE WHEN distMin IS NULL THEN
            d0 := p0 <-> p;
            d1 := p1 <-> p;
            CASE WHEN d0 < d1 THEN distMin := d0; ELSE distMin := d1; END CASE;
        ELSE NULL;
        END CASE;
        RETURN distMin;
    END;
$$ LANGUAGE plpgsql IMMUTABLE STRICT;
COMMENT ON FUNCTION qbezier_distance(point, point, point, point) IS
    'Find the distance from a point to a quadratic bezier curve.

    Adapted from http://blog.gludion.com/2009/08/distance-to-quadratic-bezier-curve.html';

CREATE OR REPLACE FUNCTION third_degree_equation(a decimal, b decimal, c decimal, d decimal)
    RETURNS decimal[]
    AS $$
    DECLARE
        ε decimal := 0.0000001;
        z decimal;  -- register, god help me
        p decimal;
        q decimal;
        p3 decimal;
        D2 decimal;
        offs decimal;
        u decimal;
        v decimal;
    BEGIN
        CASE WHEN abs(a) > ε THEN
            z := a;
            a := b / z;
            b := c / z;
            c := d / z;
            p := b - a * a / 3;
            q := a * (2 * a * a - 9 * b) / 27 + c;
            p3 := p * p * p;
            D2 := q * q + 4 * p3 / 27;
            offs := -a / 3;
            CASE WHEN D2 > ε THEN
                z := SQRT(D2);
                u := (-q + z) / 2;
                v := (-q - z) / 2;
                CASE WHEN u >= 0 THEN u := cbrt(u); ELSE u := -cbrt(-u); END CASE;
                CASE WHEN v >= 0 THEN v := cbrt(v); ELSE v := -cbrt(-v); END CASE;
                RETURN ARRAY [u + v + offs];
            WHEN D2 < -ε THEN
                u := 2 * SQRT(-p / 3);
                v := ACOS(-SQRT(-27 / p3) * q / 2) / 3;
                RETURN ARRAY [
                    u * COS(v) + offs,
                    u * COS(v + 2 * PI() / 3) + offs,
                    u * COS(v + 4 * PI() / 3) + offs
                ];
            ELSE
                CASE WHEN q < 0 THEN u := cbrt(-q / 2); ELSE u := -cbrt(q / 2); END CASE;
                RETURN ARRAY [2 * u + offs, -u + offs];
            END CASE;
        ELSE
            a := b;
            b := c;
            c := d;
            CASE WHEN abs(a) <= ε THEN
                CASE WHEN abs(b) <= ε THEN RETURN NULL;
                ELSE
                    RETURN ARRAY [-c / b];
                END CASE;
            END CASE;
            D2 := b * b - 4 * a * c;
            CASE WHEN D2 < -ε THEN RETURN NULL;
            WHEN D2 >= ε THEN
                D2 := SQRT(D2);
                RETURN ARRAY [
                    (-b - D2) / (2 * a),
                    (-b + D2) / (2 * a)
                ];
            ELSE RETURN ARRAY [-b / (2 * a)];
            END CASE;
        END CASE;
    END;
$$ LANGUAGE plpgsql IMMUTABLE STRICT;
COMMENT ON FUNCTION third_degree_equation(decimal, decimal, decimal, decimal) IS
    'Find the solutions to the cubic function `ax3 + bx2 + cx + d = 0`

    Adapted from http://blog.gludion.com/2009/08/distance-to-quadratic-bezier-curve.html';


CREATE OR REPLACE FUNCTION bezier_point(t decimal, p0 point, pc point, p1 point)
    RETURNS point
    AS $$
    BEGIN
        RETURN point (
            round((1 - t) * (1 - t) * p0[0] + 2 * (1 - t) * t * pc[0] + t * t * p1[0]),
            round((1 - t) * (1 - t) * p0[1] + 2 * (1 - t) * t * pc[1] + t * t * p1[1])
        );
    END;
$$ LANGUAGE plpgsql IMMUTABLE STRICT;


CREATE OPERATOR @> (
    leftarg = svgpath,
    rightarg = point,
    procedure = svgpath_contains
);

CREATE OR REPLACE FUNCTION to_ppm(width int, height int, shapes render_shape[], bg_color bytea default '\x000000')
    RETURNS bytea
    LANGUAGE plpgsql
AS $$
BEGIN
    RETURN format(E'P6\n%s %s\n255\n', width, height)::bytea || string_agg(px_color, ''::bytea)
    FROM generate_series(0, width - 1) as x, generate_series(0, height - 1) as y
    JOIN LATERAL (
        SELECT COALESCE(first(shape.color), bg_color) px_color
        FROM unnest(shapes) shape WHERE shape @> point (x, y)
    ) _ ON true;
END $$;

CREATE OR REPLACE FUNCTION to_ppm(width int, height int, shapes svgpath[], bg_color bytea default '\x000000')
    RETURNS bytea
    LANGUAGE plpgsql
AS $$
BEGIN
    RETURN format(E'P6\n%s %s\n255\n', width, height)::bytea || string_agg(px_color, ''::bytea)
    FROM generate_series(0, width - 1) as x, generate_series(0, height - 1) as y
    JOIN LATERAL (
        SELECT COALESCE(first(shape.color), bg_color) px_color
        FROM unnest(shapes) shape WHERE shape @> point (x, y)
    ) _ ON true;
END $$;

-- SELECT encode(to_ppm(
--     200, 400,
--     ARRAY[
--         (circle '<(100, 175) 100>', bytea '\x00FF00')::render_shape,
--         (circle '<(75, 75) 50>', bytea '\xFF0000')::render_shape
--     ]
-- ), 'base64');
