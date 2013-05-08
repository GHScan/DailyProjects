
varying vec2 g_pos;
void main() {
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
    gl_TexCoord[0] = gl_MultiTexCoord0;
    g_pos = gl_Position.xy;
}
