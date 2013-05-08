uniform sampler2D tex;
uniform vec2 g_points[4];
uniform vec2 g_uvs[4];
varying vec2 g_pos;
void main() {
    float areas[4];
    for (int i = 0; i < 4; ++i) {
        int ni = i == 3 ? 0 : i + 1; // it can't write : int ni = (i + 1) % 4;
        vec2 d0 = g_pos - g_points[i], d1 = g_points[ni] - g_points[i];
        areas[i] = cross(vec3(d1, 0), vec3(d0, 0)).z;
    }
    // these should be passing as uniform
    float pointArea[4];
    for (int i = 0; i < 4; ++i) {
        int pi = i == 0 ? 3 : i - 1;
        int ni = i == 3 ? 0 : i + 1;
        vec2 d0 = g_points[pi] - g_points[i], d1 = g_points[ni] - g_points[i];
        pointArea[i] = cross(vec3(d1, 0), vec3(d0, 0)).z;
        //pointArea[i] = float(1);
    }

    float ws[4];
    float tws = float(0);
    for (int i = 0; i < 4; ++i) {
        int ni = i == 3 ? 0 : i + 1;
        int nni = ni == 3 ? 0 : ni + 1;
        ws[i] = areas[ni] * areas[nni] * pointArea[i];
        tws += ws[i];
    }

    vec2 uv = vec2(0);
    for (int i = 0; i < 4; ++i) {
        uv += g_uvs[i] * ws[i];
    }
    uv /= tws;

    gl_FragColor = texture2D(tex, uv);
}
