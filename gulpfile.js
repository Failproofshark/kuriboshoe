var gulp = require("gulp"),
    concat = require("gulp-concat"),
    uglify = require("gulp-uglify"),
    sourcemaps = require("gulp-sourcemaps");

var adminSources = ["static/js/gametrackershared.js", "static/js/gameform.js", "static/js/gameformview.js", "static/js/adminmodels.js", "static/js/adminviews.js", "static/js/adminvmcontroller.js", "static/js/select2mithril.js", "static/js/gametrackeradmin.js"];
var clientSources = [""];
gulp.task("buildadmindebug", function() {
    return gulp.src(adminSources)
        .pipe(sourcemaps.init())
        .pipe(concat("admin.js"))
        .pipe(sourcemaps.write())
        .pipe(gulp.dest("static/js/"));
});
gulp.task("buildclientdebug", function() {
});
