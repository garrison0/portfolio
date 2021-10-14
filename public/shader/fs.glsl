#version 300 es
#define AA 1

precision highp float;

in vec2 vUv;
uniform vec2 uResolution;
uniform float uTime;
uniform float uKIFSRandom;
uniform float uRandom;

out vec4 o_FragColor;

float sizeSignal(float time) { 
    float length = 60.0; 
    float a = mod(time, length);
    float ap = 0.5 + 0.5 * sin( ( 2.0*3.141593653 / length ) * a );
    return ap;
}

/*
 
                                                    
                                                    
 `7MN.   `7MF' .g8""8q. `7MMF' .M"""bgd `7MM"""YMM  
   MMN.    M .dP'    `YM. MM  ,MI    "Y   MM    `7  
   M YMb   M dM'      `MM MM  `MMb.       MM   d    
   M  `MN. M MM        MM MM    `YMMNq.   MMmmMM    
   M   `MM.M MM.      ,MP MM  .     `MM   MM   Y  , 
   M     YMM `Mb.    ,dP' MM  Mb     dM   MM     ,M 
 .JML.    YM   `"bmmd"' .JMML.P"Ybmmd"  .JMMmmmmMMM 
                                                    
                                                    
 
*/
float mod289(float x){return x - floor(x * (1.0 / 289.0)) * 289.0;}
vec4 mod289(vec4 x){return x - floor(x * (1.0 / 289.0)) * 289.0;}
vec4 perm(vec4 x){return mod289(((x * 34.0) + 1.0) * x);}

float noise(vec3 p){
    vec3 a = floor(p);
    vec3 d = p - a;
    d = d * d * (3.0 - 2.0 * d);

    vec4 b = a.xxyy + vec4(0.0, 1.0, 0.0, 1.0);
    vec4 k1 = perm(b.xyxy);
    vec4 k2 = perm(k1.xyxy + b.zzww);

    vec4 c = k2 + a.zzzz;
    vec4 k3 = perm(c);
    vec4 k4 = perm(c + 1.0);

    vec4 o1 = fract(k3 * (1.0 / 41.0));
    vec4 o2 = fract(k4 * (1.0 / 41.0));

    vec4 o3 = o2 * d.z + o1 * (1.0 - d.z);
    vec2 o4 = o3.yw * d.x + o3.xz * (1.0 - d.x);

    return o4.y * d.y + o4.x * (1.0 - d.y);
}

//// 3d SIMPLEX NOISE /////
vec3 mod289(vec3 x) {
    return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec4 permute(vec4 x) {
    return mod289(((x*34.0)+1.0)*x);
}

vec4 taylorInvSqrt(vec4 r)
{
    return 1.79284291400159 - 0.85373472095314 * r;
}

const mat2 m2 = mat2( 0.60, -0.80, 0.80, 0.60 );

const mat3 m3 = mat3( 0.00,  0.80,  0.60,
                     -0.80,  0.36, -0.48,
                     -0.60, -0.48,  0.64 );

float fbm( in vec3 p ) {
    float f = 0.0;
    f += 0.5000*noise( p ); p = m3*p*2.02;
    f += 0.2500*noise( p ); p = m3*p*2.03;
    f += 0.1250*noise( p ); p = m3*p*2.01;
    f += 0.0625*noise( p );
    return f/0.9375;
}

/*
 
                                       
                                       
  .M"""bgd `7MM"""Yb. `7MM"""YMM       
 ,MI    "Y   MM    `Yb. MM    `7       
 `MMb.       MM     `Mb MM   d ,pP"Ybd 
   `YMMNq.   MM      MM MM""MM 8I   `" 
 .     `MM   MM     ,MP MM   Y `YMMMa. 
 Mb     dM   MM    ,dP' MM     L.   I8 
 P"Ybmmd"  .JMMmmmdP' .JMML.   M9mmmP' 
                                       
                                       
 
*/

vec2 opU( vec2 d1, vec2 d2 )
{
	return (d1.x<d2.x) ? d1 : d2;
}

vec2 opSmoothU( vec2 d1, vec2 d2, float k) 
{ 
    float colorSmoothness = k * 4.0;
    float interpo = clamp( 0.5 + 0.5 * (d1.x - d2.x) / colorSmoothness, 0.0, 1.0 );
    float h = max( k - abs(d1.x - d2.x), 0.0) / k;
    float diff = h*h*h*k*(1.0/6.0);
    return vec2( min(d1.x, d2.x) - diff,
                 mix(d1.y, d2.y, interpo) - k * interpo * ( interpo - 1.0) );
}

float opSmoothSubtraction( float d1, float d2, float k ) {
    float h = clamp( 0.5 - 0.5*(d2+d1)/k, 0.0, 1.0 );
    return mix( d2, -d1, h ) + k*h*(1.0-h); }

float opIntersection( float d1, float d2 ) { return max(d1,d2); }

float sdBox( vec3 p, vec3 b )
{
  vec3 q = abs(p) - b;
  return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
}


/*
 
                                                                                      
                                                                                      
 `7MMM.     ,MMF'      db      `7MM"""Mq.`7MM"""Mq.`7MMF'`7MN.   `7MF' .g8"""bgd      
   MMMb    dPMM       ;MM:       MM   `MM. MM   `MM. MM    MMN.    M .dP'     `M      
   M YM   ,M MM      ,V^MM.      MM   ,M9  MM   ,M9  MM    M YMb   M dM'       `      
   M  Mb  M' MM     ,M  `MM      MMmmdM9   MMmmdM9   MM    M  `MN. M MM               
   M  YM.P'  MM     AbmmmqMA     MM        MM        MM    M   `MM.M MM.    `7MMF'    
   M  `YM'   MM    A'     VML    MM        MM        MM    M     YMM `Mb.     MM      
 .JML. `'  .JMML..AMA.   .AMMA..JMML.    .JMML.    .JMML..JML.    YM   `"bmmmdPY      
                                                                                      
                                                                                      
 
*/

vec3 rotatePoint(vec3 p, vec3 n, float theta) { 
    vec4 q = vec4(cos(theta / 2.0), sin (theta / 2.0) * n);
    vec3 temp = cross(q.xyz, p) + q.w * p;
    vec3 rotated = p + 2.0*cross(q.xyz, temp);
    return rotated;
}

vec2 mapFlux(vec3 p, float time, vec3 pos) { 
    vec2 res = vec2(1e10, 0.0);

    {
        float NoiseScale = 3.0;
        float NoiseIsoline = 0.5;
        vec3 p2 = p / NoiseScale + time * vec3(0.5);
        float noise = (0.5+0.6*sizeSignal(time*0.5)) * NoiseScale * (fbm( p2 ) - NoiseIsoline);
        p.y -= noise;

        float d = sdBox(p + vec3(0.5,0.5,0.0), vec3(1.0, 0.05, 1.0)) - 0.05 * ( sizeSignal(time) );
        res = opSmoothU(res, vec2(d, 15.0), 2.25);
    }

    res.x *= 0.25;
    return res;
}

vec2 fanOut(vec2 coords, float divisions) {
    float k = 2.0*3.141592653 / divisions;
    float angle = k * -floor(atan(coords.y, coords.x) / k);
    float c = cos(angle), s = sin(angle);
    mat2 rot = mat2(c, -s, s, c);
    coords *= rot;
    return coords;
}

vec2 map (vec3 p, float time) { 
    vec2 res = vec2(1e10, 0.0);
    float boxDistance = 5.0;

    vec3 q = p;
    q -= vec3(0.0,2.25,-27.5);
    float sc = 1.0;
    {
        time *= 0.2;

        mat3 kifsRot = mat3(1.0, 0, 0, 0, 1.0, 0, 0, 0, 1.0);
        float offset = -1.25 - uRandom * 0.5*(0.5+0.5*sin(time)) - (1.0 - uRandom) * (0.5+0.5*cos(cos(cos(time)))); // nice for box!
        // float offset = -3.0 + 3.1*uRandom*(0.5+0.5*sin(sin(time)));
        
        float a = sin(1.54*time) * 1.25 + uRandom * 1.25; // nice for box
//        float a = sin(time) * 2.25 + 2.25;
        float s = sin(a), c = cos(a);
        kifsRot = kifsRot * mat3(c, -s, 0, s, c, 0, 0, 0, 1);
        kifsRot = kifsRot * mat3(1, 0, 0, 0, c, -s, 0, s, c);
        kifsRot = kifsRot * mat3(c, 0, s, 0, 1, 0, -s, 0, c);
        
        int ITERS = 10 + int(floor(uKIFSRandom * 10.0));
        for (int i = 0; i < ITERS; ++i) { //
            q.xy = fanOut(q.yx, 2.0); // 1.0 looked nice - 2.0 worked best
            // q.xz = abs(q.xz); //just q
            if (uKIFSRandom < 0.5) {
                q.xz = abs(q.zx);
            } else { 
                q.xy = abs(q.xy);
            }
            q += offset / float(ITERS);
            q = rotatePoint(q, normalize(vec3( 0.5+0.5*cos(0.5*time * uKIFSRandom), 0.5+0.5*sin(0.5*time), 1.0) ), time);
            
            q = kifsRot * q;
            sc *= (1.0 + (1.0 / float(ITERS)));
        }

        // res.x *= 0.08;
        res.x *= 1.0 / (sc);
    }

    res = opSmoothU(res, vec2(sdBox(q, vec3(0.75) / (sc) ), 5.0), 0.01);

    return res;
}

vec2 raycast (in vec3 ro, in vec3 rd, float time){
    vec2 res = vec2(1e10,-1.0);

    float tmin = 0.0021;
    float tmax = 100.0;

    float eps = 0.005;
    float t = tmin;
    for( int i = 0; i < 220 && t < tmax; i++) {
        vec2 h = map( ro + rd*t, time );

        if( abs(h.x) < eps){
            res = vec2(t, h.y);
            break;
        } 

        res.x = min(res.x, h.x);

        t += h.x;
    }
    return res;
}

/*
                                                                                             
                                                                                             
 `7MMF'   `7MF'MMP""MM""YMM `7MMF'`7MMF'      `7MMF'MMP""MM""YMM `7MMF'`7MM"""YMM   .M"""bgd 
   MM       M  P'   MM   `7   MM    MM          MM  P'   MM   `7   MM    MM    `7  ,MI    "Y 
   MM       M       MM        MM    MM          MM       MM        MM    MM   d    `MMb.     
   MM       M       MM        MM    MM          MM       MM        MM    MMmmMM      `YMMNq. 
   MM       M       MM        MM    MM      ,   MM       MM        MM    MM   Y  , .     `MM 
   YM.     ,M       MM        MM    MM     ,M   MM       MM        MM    MM     ,M Mb     dM 
    `bmmmmd"'     .JMML.    .JMML..JMMmmmmMMM .JMML.   .JMML.    .JMML..JMMmmmmMMM P"Ybmmd"  
                                                                                             
                                                                                            
*/
vec3 palette( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d )
{  return a + b*cos( 6.28318*(c*t+d) ); }

vec3 calcNormal( in vec3 p, float time )
{
    const float eps = 0.0001; 
    const vec2 h = vec2(eps,0);
    return normalize( vec3(map(p+h.xyy, time).x - map(p-h.xyy, time).x,
                        map(p+h.yxy, time).x - map(p-h.yxy, time).x,
                        map(p+h.yyx, time).x - map(p-h.yyx, time).x ) );
}

float calcAO( in vec3 pos, in vec3 nor, float time )
{
	float occ = 0.0;
    float sca = 1.0;
    for( int i=0; i<5; i++ )
    {
        float h = 0.01 + 0.12*float(i)/4.0;
        float d = map( pos + h*nor, time ).x;
        occ += (h-d)*sca;
        sca *= 0.95;
        if( occ>0.35 ) break;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 ) * (0.5+0.5*nor.y);
}

float calcSoftshadow( in vec3 ro, in vec3 rd, float mint, float maxt, float k, float time )
{
    float res = 1.0;
    for( float t=mint; t<maxt; )
    {
        float h = map(ro + rd*t, time).x;
        if( h< mint )
            return 0.0;
        res = min( res, k*h/t );
        t += h;
    }
    return res;
}

/*
 
                                                                                                            
                                                                                                            
 `7MM"""Mq.  `7MM"""YMM  `7MN.   `7MF'`7MM"""Yb. `7MM"""YMM  `7MM"""Mq.  `7MMF'`7MN.   `7MF' .g8"""bgd      
   MM   `MM.   MM    `7    MMN.    M    MM    `Yb. MM    `7    MM   `MM.   MM    MMN.    M .dP'     `M      
   MM   ,M9    MM   d      M YMb   M    MM     `Mb MM   d      MM   ,M9    MM    M YMb   M dM'       `      
   MMmmdM9     MMmmMM      M  `MN. M    MM      MM MMmmMM      MMmmdM9     MM    M  `MN. M MM               
   MM  YM.     MM   Y  ,   M   `MM.M    MM     ,MP MM   Y  ,   MM  YM.     MM    M   `MM.M MM.    `7MMF'    
   MM   `Mb.   MM     ,M   M     YMM    MM    ,dP' MM     ,M   MM   `Mb.   MM    M     YMM `Mb.     MM      
 .JMML. .JMM..JMMmmmmMMM .JML.    YM  .JMMmmmdP' .JMMmmmmMMM .JMML. .JMM..JMML..JML.    YM   `"bmmmdPY      
                                                                                                            
                                                                                                            
    lighting, raymarching. 
*/

vec2 csqr( vec2 a )  { return vec2( a.x*a.x - a.y*a.y, 2.*a.x*a.y  ); }

float mapMarble( vec3 p, float time ) { 
	float res = 0.;

    p *= 0.06;
    vec3 c = p;
    c.y *= 2.5;
	for (int i = 0; i < 12; ++i) {
        p =.82*abs(p)/dot(p,p)-0.82;
        p.yz= csqr(p.yz);
        p=p.zxy;
        res += exp(-25. * abs(dot(p,c)));
        
	}
	return res/2.;
}

vec3 marchMarbleColor(vec3 ro, vec3 rd, float tmin, float tmax, float time) { 
    float t = tmin;
    float dt = .01;
    vec3 col= vec3(0.);
    float c = 0.;
    for( int i=0; i<14; i++ )
	{
        t+=dt*exp(-2.*c);
        if(t>tmax)break;
        
        c = mapMarble(ro+t*rd, time);               

        col = .99*col+ .1*vec3(c, c*c, c*c*c);
    }    
    return col;
}

vec3 render(in vec3 ro, in vec3 rd, in vec3 rdx, in vec3 rdy, float time) { 
    vec3 col = vec3(.424, .302, .435);
    col = pow(col, vec3(1.0/0.4545));

    vec2 res = raycast(ro,rd, time);
    float t = res.x;
    float m = res.y;

    vec3 pos = ro + rd*t;
    vec3 nor = calcNormal(pos, time);

    vec3 material = vec3(0);
    vec2 K = vec2(0,1); // amount, power for specular
    vec3 f0 = vec3(0.05);
    float rou = 1.0; // roughness
    vec3 hNor = vec3(0); // microfacet normal - is getting it from texture better than 'hal'? 
    float a = 1.0;

    // materials
    // blob
    if (m < 20.) {
        material = marchMarbleColor(pos, rd, 0.021, 10.0, 0.1*time);
        hNor = nor;
        rou = 1.0;
        K = vec2(0.95, 16.0);
        f0 = vec3(.972, .961, .915);
        a = pow(rou, 2.0);
    } 

    // lighting
    if ( m > 0. ) { 
        material *= 0.72 * material;
        
        float occ = calcAO(pos, nor, time);
        a = 0.5*a + 0.5*((2.0 / pow(rou, 2.0)) - 2.0);
        
        float bou = clamp( 0.3-0.7*nor.y, 0.0, 1.0 );
        vec3 ref = reflect( rd, nor );
        vec3 lin = vec3(0);

        // indoor lighting 
        // top - BRDF
        {
            vec3 lig = normalize(vec3(0.2,1.0,0.05));

            float dif = clamp(dot(lig,nor), 0.0, 1.0);
            dif *= occ;

            vec3 hal  = normalize(lig - rd);
            float fre = clamp(1.0 - dot(lig, hal), 0.0, 1.0);

            vec3 clr = normalize(vec3(0.5, 0.633, 0.9));
            // float speBias = smoothstep(0.3, 0.42, ref.y); // to make it seem more like an area light

            // fresnel
            vec3 fSch = f0 + (vec3(1) - f0)*pow(fre, 5.0);  
            
            // distribution
            float dBlinnPhong = ((a + 2.0) / (2.0*3.141592653)) * pow(clamp(dot(nor, hNor), 0.0, 1.0), a);

            // full spectral light addition
            vec3 spe = (fSch * dBlinnPhong) / 2.0;

            lin += K.x * 0.65 * spe * dif * clr; // spec
            lin += (1.0 - K.x) * 2.75 * dif * clr * material; // dif
        }
        //side 
        {
            vec3 lig = normalize(vec3(-0.5, 0.3, 0.1));
            float dif = 0.1 + 0.9 * clamp(dot(lig, nor), 0.0, 1.0);

            vec3 clr = vec3(1.0, 0.6, 0.5);
            dif *= occ;

            vec3 hal  = normalize(lig - rd);
            float fre = clamp(1.0 - dot(lig, nor), 0.0, 1.0); 

            vec3 spe = vec3(1)*(pow(clamp(dot(nor,hal), 0.0, 1.0), a / 2.0));

            vec3 fSch = f0 + (vec3(1) - f0)*pow(fre, 5.0);   
            spe *= fSch;

            lin += K.x * 0.75 * spe * dif * clr;
            lin += (1.0 - K.x) * 3.5 * dif * clr * material;
        }
        // back (?)
        // below - bounce
        {
            lin += 2.0 * material * vec3(0.8,0.4,0.45) * occ + 2.0 * material * vec3(0.5,0.41,0.39) * bou * occ;
        }
        // sss
        {
            vec3 lig = normalize(vec3(0.2,1.0,0.05));
            vec3 hal  = normalize(lig - rd);
            float dif = clamp(dot(nor, hal), 0.0, 1.0);
            float fre = clamp(1.0 - dot(lig, hal), 0.0, 1.0);
            vec3 fSch = f0 + (vec3(1) - f0)*pow(fre, 5.0);   
            lin += 3.5 * (1.0 - K.x) * fre * fre * (0.2 + 0.8 * dif * occ) * material;
        }
        
        col = (0.05 * material) + (0.95 * lin);
    } 

    if (m < 0.0) { 
        col += vec3(0.65,0.7,1.0)*clamp( 100.0*pow(exp( -3.0 - t ),2.3), 0.0, 1.0);
    }
    
    return vec3( clamp(col, 0.0, 1.0) );
}

mat3 setCamera( in vec3 ro, in vec3 ta, float cr )
{
    vec3 cw = normalize(ta-ro);
    vec3 cp = vec3(sin(cr), cos(cr),0.0);
    vec3 cu = normalize( cross(cw,cp) );
    vec3 cv =          ( cross(cu,cw) );
    return mat3( cu, cv, cw );
}

void main() {
    // camera
    vec3 ro = vec3( 0.0, 4.2, 1.5);
    vec3 ta = vec3( 0.0, 4.11, 0.5);

    mat3 ca = setCamera(ro, ta, 0.0);

    float aspect = uResolution.x / uResolution.y;

    vec3 total = vec3(0.0);
#if AA>1
    for (int m=0; m < AA; m++)
    for (int n=0; n < AA; n++) { 
        vec2 o = (vec2(float(m), float(n)) / uResolution) / float(AA);
        vec2 p = vec2(aspect, 1.0) * ( (vUv+o) - vec2(0.5));
        float time = uTime + 1.0 * (1.0/48.0) * float(m * n * AA) / float(AA*AA);
#else
        vec2 p = vec2(aspect, 1.0) * (vUv - vec2(0.5));
        float time = uTime;
#endif

        // ray direction
        vec3 rd = ca * normalize( vec3(p, 2.2) );

        // ray differentials 
        vec2 px =  vec2(aspect, 1.0) * ( (vUv+vec2(1.0,0.0)) - vec2(0.5));
        vec2 py =  vec2(aspect, 1.0) * ( (vUv+vec2(0.0,1.0)) - vec2(0.5));
        vec3 rdx = ca * normalize( vec3(px, 2.5));
        vec3 rdy = ca * normalize( vec3(py, 2.5));

        vec3 color = render( ro, rd, rdx, rdy, time );

        color = pow(color, vec3(0.4545));

        total += color;
#if AA>1
    }
    total /= float(AA*AA);
#endif
    
    total = min(total, 1.0);

    o_FragColor = vec4( total, 1.0 );
}