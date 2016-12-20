// Less configuration
var gulp = require("gulp");
var less = require("gulp-less");

gulp.task("less", function() {
  gulp.src("*.less").pipe(less()).pipe(gulp.dest(function(f) {
    return f.base;
  }))
});

gulp.task("default", ["less"], function() {
  gulp.watch("*.less", ["less"]);
})