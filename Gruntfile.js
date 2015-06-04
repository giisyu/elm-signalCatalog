
module.exports = function(grunt) {

  grunt.initConfig({

    watch: {
      elm: {
        files: ["signalCatalog.elm"],
        tasks: ["elm"]
      }
    },


    elm: {
      dev :{
        files: {
          "./elm.js": "./signalCatalog.elm"
        }
      }
    },

    browserSync: {
      dev: {
        bsFiles : {
          src : "elm.js"
        },
          options: {
          watchTask: true,
          port: 8000,
          server:{
            baseDir : "./",
            index : "index.html"
          }
        }
      }
    },

  });

  ["grunt-contrib-watch" , "grunt-elm" , "grunt-browser-sync"].forEach(function(plugin) {
    grunt.loadNpmTasks(plugin);
  });

  grunt.registerTask("default", [ "elm", "browserSync:dev", "watch"]);
};