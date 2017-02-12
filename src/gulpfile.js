// Less configuration
var gulp = require("gulp");
var less = require("gulp-less");

function parentDir(f) {
  const curr = "src";
  const dest = f.base.slice(0, -(curr.length+1));
  console.log("to parent dir:" + dest);
  return dest;
}

gulp.task("less", function() {
  gulp.src("*.less").pipe(less()).pipe(gulp.dest(parentDir))
});

gulp.task("default", ["less"], function() {
  gulp.watch("*.less", ["less"]);
})