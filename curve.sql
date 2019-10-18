-- SELECT create_svgpath('M 100 350 q 150 -300 300 0', 5, bytea '\x0000FF');

-- SELECT encode(to_ppm(
--     450, 400,
--     ARRAY[
--         create_svgpath('M 100 350 q 150 -300 300 0 Z', 5, bytea '\x0000FF')
--     ]
-- ), 'base64');

-- SELECT encode(to_ppm(
--     200, 100,
--     ARRAY[
--         create_svgpath('M 10,50
--            Q 25,25 40,50
--            t 30,0 30,0 30,0 30,0 30,0', 5, bytea '\x0000FF')
--     ]
-- ), 'base64');

-- SELECT encode(to_ppm(
--     200, 100,
--     ARRAY[
--         create_svgpath('M 10,50
--            Q 25,25 40,50
--            t 30,0', 5, bytea '\x0000FF')
--     ]
-- ), 'base64');

SELECT encode(to_ppm(
    450, 400,
    ARRAY[
        create_svgpath('M 100 350 L 150 300 200 325 Z', 5, bytea '\x0000FF')
    ]
), 'base64');

-- SELECT bezier_points(100, 350, 250, 50, 400, 350);

-- SELECT create_svgpath('M 100 350 q 150 -300 300 0', 5, bytea '\x0000FF') @> point (;
