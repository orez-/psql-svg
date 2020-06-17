SET SESSION SCHEMA 'ppm';

-- === QUADRATIC BEZIER CURVE ===
-- SELECT encode(to_ppm(
--     450, 400,
--     ARRAY[
--         create_svgpath('M 100 350 q 150 -300 300 0', 5, bytea '\x0000FF')
--     ]
-- ), 'base64');

-- === SIN WAVES ===
SELECT encode(to_ppm(
    200, 200,
    ARRAY[
        create_svgpath('M 10,50
           Q 25,25 40,50
           t 30,0 30,0 30,0 30,0 30,0', 5, bytea '\x0000FF'),
        create_svgpath('M 10, 150
            q 15 25 30 0
            t 30,0 30,0 30,0 30,0 30,0', 5, bytea '\xFF0000')
    ]
), 'base64');

-- === FIRST PERIOD OF A SIN WAVE ===
-- SELECT encode(to_ppm(
--     200, 100,
--     ARRAY[
--         create_svgpath('M 10,50
--            Q 25,25 40,50
--            t 30,0', 5, bytea '\x0000FF')
--     ]
-- ), 'base64');

-- === TRIANGLE ===
-- SELECT encode(to_ppm(
--     450, 400,
--     ARRAY[
--         create_svgpath('M 100 350 L 150 300 200 325 Z', 5, bytea '\x0000FF')
--     ]
-- ), 'base64');
