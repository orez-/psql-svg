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

-- this function sucks but let's prototype this mofo out
CREATE OR REPLACE FUNCTION bezier_points(p0 point, pc point, p1 point)
    RETURNS SETOF point
    AS $$
    BEGIN
    RETURN QUERY SELECT point(x, y) FROM (SELECT DISTINCT
        round((1 - t.v) * (1 - t.v) * p0[0] + 2 * (1 - t.v) * t.v * pc[0] + t.v * t.v * p1[0]) as x,
        round((1 - t.v) * (1 - t.v) * p0[1] + 2 * (1 - t.v) * t.v * pc[1] + t.v * t.v * p1[1]) as y
    FROM (SELECT x::decimal / 100. as v FROM generate_series(0, 100) as x) as t) _;
    END;
$$ LANGUAGE plpgsql IMMUTABLE STRICT;

CREATE OR REPLACE FUNCTION svgpath_contains(svgpath, point)
    RETURNS bool
    AS $$
    BEGIN
        RETURN EXISTS (
            SELECT 1
            FROM unnest(($1).steps) step
            WHERE (
                SELECT CASE (step).svg_command
                    WHEN 'Q' THEN EXISTS (
                        SELECT 1
                        FROM bezier_points((step).args[1], (step).args[2], (step).args[3]) _(p)
                        WHERE circle (p, ($1).width) @> $2
                    )
                    WHEN 'L' THEN $2 <-> lseg((step).args[1], (step).args[2]) <= ($1).width
                END
            )
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
