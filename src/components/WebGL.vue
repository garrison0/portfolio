<template>
  <div class="hack">
    <canvas class="rounded-b-lg" id="c"></canvas>
  </div>
</template>

<script>
  import * as twgl from '../../node_modules/twgl.js/dist/4.x/twgl-full.module.js';

  export default {
    name: 'WebGL',

    mounted () {
      this.runWebGL()
    },

    methods: {
      runWebGL(){
        const gl = document.querySelector("#c").getContext("webgl2");
        var state = { mousePos: [0.5, 0.5], images: {}, textures: {}, random: Math.random(), kifsRandom: Math.random() };

        function init(preloaded)
        {
            state.programInfo = twgl.createProgramInfo(gl, [preloaded['vs'], preloaded['fs']]);

            state.bufferInfo = twgl.primitives.createXYQuadBufferInfo(gl);

            for (const [fileName,image] of Object.entries(state.images)) { 
              state.textures[fileName] = gl.createTexture();
              gl.bindTexture(gl.TEXTURE_2D, state.textures[fileName]);
              const level = 0;
              const internalFormat = gl.RGB;
              const srcFormat = gl.RGB;
              const srcType = gl.UNSIGNED_BYTE;
              gl.texImage2D(gl.TEXTURE_2D, level, internalFormat,
                          srcFormat, srcType, image);
              gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);
              gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);
              gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
            }

            gl.enable(gl.BLEND);
            gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
            gl.clearColor(0.0,0.0,0.0,1.0);

            requestAnimationFrame(render);
        }

        function render(time) { 
            time *= 0.001;

            twgl.resizeCanvasToDisplaySize(gl.canvas, 1.0);
            gl.viewport(0, 0, gl.canvas.clientWidth, gl.canvas.clientHeight);

            var uniforms = {
                uTime: time,
                uResolution: [gl.canvas.clientWidth, gl.canvas.clientHeight],
                uRandom: state.random,
                uKIFSRandom: state.kifsRandom
            }

            for (const [textureName, texture] of Object.entries(state.textures)) { 
              let uniformName = 'u' + textureName.charAt(0).toUpperCase() + textureName.slice(1);
              uniforms[uniformName] = texture;
            }

            gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

            gl.useProgram(state.programInfo.program);
            twgl.setUniforms(state.programInfo, uniforms);
            twgl.setBuffersAndAttributes(gl, state.programInfo, state.bufferInfo); 
            twgl.drawBufferInfo(gl, state.bufferInfo);

            requestAnimationFrame(render);
        }


        // -------- HANDLE BROWSER EVENTS / INTERACTIVITY             --------- //
        document.onmousemove = (event) => { 
            const rect = gl.canvas.getBoundingClientRect();
            const x = (event.clientX - rect.left) / gl.canvas.width * 2 - 1;
            const y = (event.clientY - rect.top ) / gl.canvas.height * 2 - 1;
            state.mousePos = [2 * x,2 * -y];
        }

        // -------- FETCH FILES ( SHADERS, IMAGES FOR TEXTURES, ETC ) --------- //
        // keys correspond to the file names of each shader
        var files = {'fs': '', 'vs': ''};
        var count = 0;
        let numFiles = 2;

        for (const key of Object.keys(files)) { 
          fetch('./shader/' + key + '.glsl')
            .then(response => response.text())
            .then(text => {
              files[key] = text;
              count = count + 1;
              if (count >= numFiles) 
                init(files);
            });
        }
      }
    }
  }
</script>

<style scoped>
  #c { 
    width: 100%;
    height: 30rem;
  }

  /* for some reason, 7px are left at the bottom of the div below the canvas */
  .hack {
    margin-bottom: -7px;
  }

</style>
