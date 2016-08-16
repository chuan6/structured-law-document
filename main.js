function px(x) {
    return "" + x + "px";
}

function horizontalExtra(computed) {
    var x =  parseFloat(computed.marginLeft)
        + parseFloat(computed.marginRight)
        + parseFloat(computed.borderLeftWidth)
        + parseFloat(computed.borderRightWidth)
        + parseFloat(computed.paddingLeft)
        + parseFloat(computed.paddingRight);
    return x;
}

function getEnclosingID(elmt) {
    return elmt.id? elmt.id : getEnclosingID(elmt.parentNode);
}

function updateHrefToID(a, id) {
    if (id) {
        a.href = "#" + id;
    } else {
        a.removeAttribute("href");
    }
}

var backButton = function (init) {
    var bb;
    var stack, top;

    bb = document.createElement("a");
    bb.id = "back-button";
    bb.textContent = "返回";
    updateHrefToID(bb, init);

    stack = [init];

    top = function () {
        return stack[stack.length - 1];
    };

    return {
        "element": bb,
        "push": function (id) {
            console.log("before push:", stack);
            if (id !== top()) {
                // only push new id that is different from the top
                stack.push(id);
                updateHrefToID(bb, id);
            }
            console.log("after push:", stack);
        },
        "pop": function () {
            console.log("before pop:", stack);
            if (stack.length > 1) {
                stack.pop();
            }
            updateHrefToID(bb, top());
            console.log("after pop:", stack);
        },
        "peek": top
    };
}("outline");

window.addEventListener("load", function () {
    document.body.appendChild(backButton.element);
});

window.addEventListener("hashchange", function (e) {
    var hash = decodeURI(window.location.hash);

    if (hash === "#" + backButton.peek()) {
        backButton.pop();
    }
});

window.addEventListener("click", function (e) {
    var id = getEnclosingID(e.target);
    var y = window.pageYOffset;

    // if the click is originated from an on screen element,
    // prevent page from scrolling after location.hash update
    if (e.target.tagName !== "A") {
        window.location.hash = "#" + id;
        window.scrollTo(0, y);
        return;
    }

    if (id !== "back-button") {
        backButton.push(id);
    }
});
