function strSlice(s, end) {
    var e = escape(s), t = "";
    var i, n;

    for (i = 0; i < end && e; i++) {
        if (e[0] === "%" && e[1] === "u") {//four-digit format
            n = 6;
        } else if (e[0] === "%") {//two-digit format
            n = 3;
        } else {//one-digit format
            n = 1;
        }
        t += e.slice(0, n);
        e = e.slice(n);
    }
    return [unescape(t), e];
}

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
    return elmt.id || (elmt.parentNode? getEnclosingID(elmt.parentNode) : null);
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

var shareButton = function () {
    var sb, text, link;

    sb = document.createElement("button");
    sb.id = "share-button";
    sb.textContent = "分享";
    sb.style.display = "none";

    return {
        "element": sb,
        "showAt": function (y) {
            var top = y + window.pageYOffset - 26;

            sb.style.top = px(top);
            sb.style.display = "block";
        },
        "setContent": function (s, ref) {
            text = s;
            link = ref;
        },
        "getContent": function () {
            var nchars = 50;
            var sliced = strSlice(text, nchars);
            return sliced[0] + (sliced[1]? "……":"") + " " + link;
        }
    };
}();

var overlay = function () {
    var ol, content, buttonPanel, docancel;

    ol = document.createElement("div");
    ol.id = "overlay";
    ol.style.display = "none";
    ol.onclick = function (e) {
        e.stopPropagation();
    };

    content = document.createElement("textarea");
    content.id = "copy-text";

    buttonPanel = document.createElement("div");
    docancel = document.createElement("button");
    docancel.textContent = "取消";
    docancel.onclick = function (e) {
        ol.style.display = "none";
        e.stopPropagation();
    };
    buttonPanel.appendChild(docancel);

    ol.appendChild(content);
    ol.appendChild(buttonPanel);

    return {
        "element": ol,
        "setContent": function (s) {
            content.textContent = s;
        },
        "show": function () {
            ol.style.display = "block";
        }
    };
}();

window.addEventListener("load", function () {
    document.body.appendChild(backButton.element);
    document.body.appendChild(shareButton.element);
    document.body.appendChild(overlay.element);
});

window.addEventListener("hashchange", function (e) {
    var hash = decodeURI(window.location.hash);

    if (hash === "#" + backButton.peek()) {
        backButton.pop();
    }
});

function editHashAndScrollLazily(hash, dontScroll) {
    var backToPrevY = function () {
        var y = window.pageYOffset;
        return function () {
            window.scrollTo(0, y);
        };
    }();

    var elmt = document.getElementById(hash.slice(1));
    var x = dontScroll? 0 : (function () {
        var rect = elmt.getBoundingClientRect();
        var h = rect.bottom - rect.top;

        if (rect.top < 0 || h > window.innerHeight) return -1;
        // rect.top >= 0 && h <= window.innerHeight

        if (rect.bottom <= window.innerHeight) return 0;
        // rect.bottom > window.innerHeight && h <= window.innerHeight

        return 1;
    })();

    window.location.hash = hash;

    switch (x) {
    case 0:
        backToPrevY();
        break;
    case -1:
        elmt.scrollIntoView(true);
        break;
    case 1:
        elmt.scrollIntoView(false);
    }
}

window.addEventListener("click", function (e) {
    var id = getEnclosingID(e.target);
    var elmt;

    if (!id) return;
    // id is truthy

    elmt = document.getElementById(id);

    if (id === "share-button") {
        overlay.setContent(shareButton.getContent());
        overlay.show();
        return;
    }

    // if the click is originated from an on screen element,
    // prevent page from scrolling after location.hash update
    if (e.target.tagName !== "A") {
        editHashAndScrollLazily(id, true);
        shareButton.showAt(elmt.getBoundingClientRect().top);
        shareButton.setContent(
            elmt.textContent,
            window.location.href);
        return;
    } // e.target.tagName === "A"

    e.preventDefault();
    if (id !== "back-button") {
        backButton.push(id);
    }
    editHashAndScrollLazily(e.target.getAttribute("href"));
});
