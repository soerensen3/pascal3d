/* VBlurVertexShader.glsl */
//attribute vec4 a_position;
//attribute vec2 a_texCoord;
 
varying vec2 v_texCoord;
varying vec2 v_blurTexCoords[14];

const float scale = 0.05;
 
void main()
{
    gl_Position = ftransform();//a_position;
    v_texCoord = gl_MultiTexCoord0.st;//a_texCoord;
    v_blurTexCoords[ 0] = v_texCoord + vec2(0.0, -0.028 *scale);
    v_blurTexCoords[ 1] = v_texCoord + vec2(0.0, -0.024 *scale);
    v_blurTexCoords[ 2] = v_texCoord + vec2(0.0, -0.020 *scale);
    v_blurTexCoords[ 3] = v_texCoord + vec2(0.0, -0.016 *scale);
    v_blurTexCoords[ 4] = v_texCoord + vec2(0.0, -0.012 *scale);
    v_blurTexCoords[ 5] = v_texCoord + vec2(0.0, -0.008 *scale);
    v_blurTexCoords[ 6] = v_texCoord + vec2(0.0, -0.004 *scale);
    v_blurTexCoords[ 7] = v_texCoord + vec2(0.0,  0.004 *scale);
    v_blurTexCoords[ 8] = v_texCoord + vec2(0.0,  0.008 *scale);
    v_blurTexCoords[ 9] = v_texCoord + vec2(0.0,  0.012 *scale);
    v_blurTexCoords[10] = v_texCoord + vec2(0.0,  0.016 *scale);
    v_blurTexCoords[11] = v_texCoord + vec2(0.0,  0.020 *scale);
    v_blurTexCoords[12] = v_texCoord + vec2(0.0,  0.024 *scale);
    v_blurTexCoords[13] = v_texCoord + vec2(0.0,  0.028 *scale);
}
