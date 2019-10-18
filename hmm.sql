SELECT encode(to_ppm(
    225, 225,
    ARRAY[
        (circle '<(81, 70) 13>', bytea '\x65471b')::render_shape,
        (circle '<(152, 76), 13>', bytea '\x65471b')::render_shape,
        (circle '<(112, 112) 107>', bytea '\xffcb4c')::render_shape
    ]
), 'base64');
