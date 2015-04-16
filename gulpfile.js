var gulp = require("gulp"),
    concat = require("gulp-concat"),
    uglify = require("gulp-uglify"),
    sourcemaps = require("gulp-sourcemaps");

var adminSources = ["static/js/gametrackershared.js", "static/js/gameform.js", "static/js/gameformview.js", "static/js/adminmodels.js", "static/js/adminviews.js", "static/js/adminvmcontroller.js", "static/js/select2mithril.js", "static/js/gametrackeradmin.js"];
var clientSources = ["static/js/select2mithril.js", "static/js/gametrackershared.js", "static/js/gameform.js", "static/js/gameformview.js", "static/js/clientmodels.js", "static/js/clientcontroller.js", "static/js/clientview.js", "static/js/gametrackerclient.js"];

gulp.task("builddebug", function() {
    gulp.start("buildadmindebug", "buildclientdebug");
});
gulp.task("buildadmindebug", function() {
    return gulp.src(adminSources)
        .pipe(sourcemaps.init())
        .pipe(concat("admin.js"))
        .pipe(sourcemaps.write())
        .pipe(gulp.dest("static/js/"));
});
gulp.task("buildclientdebug", function() {
    return gulp.src(clientSources)
        .pipe(sourcemaps.init())
        .pipe(concat("client.js"))
        .pipe(sourcemaps.write())
        .pipe(gulp.dest("static/js/"));
});

gulp.task("buildproduction", function() {
    gulp.start("buildadminproduction", "buildclientproduction");
});
gulp.task("buildclientproduction", function() {
    return gulp.src(clientSources)
        .pipe(concat("client.js"))
        .pipe(uglify())
        .pipe(gulp.dest("static/js/"));
});
gulp.task("buildadminproduction", function() {
    return gulp.src(adminSources)
        .pipe(concat("admin.js"))
        .pipe(uglify())
        .pipe(gulp.dest("static/js/"));
});
