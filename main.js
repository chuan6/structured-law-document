function px(x) {
    return "" + x + "px";
}

function getAvailHeightFn (scrollbarHeight) {
    return function () {
        return window.innerHeight - scrollbarHeight;
    };
}

window.addEventListener("load", function () {
    var container = document.getElementById("entries-container");
    var h = getAvailHeightFn(32);

    container.style.height = px(h());

    window.onresize = function () {
        container.style.height = px(h());
    };
});
