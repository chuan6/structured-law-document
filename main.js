var __assign = (this && this.__assign) || Object.assign || function(t) {
    for (var s, i = 1, n = arguments.length; i < n; i++) {
        s = arguments[i];
        for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
            t[p] = s[p];
    }
    return t;
};
function isIn(coll, x) {
    return coll.indexOf(x) === -1 ? false : true;
}
function partial(f, arg1) {
    return f.bind(null, arg1);
}
function iter(coll, g, f) {
    var i;
    for (i = 0; i < coll.length; i++) {
        g(coll[i], f(coll[i]));
    }
}
;
// See:
// https://mathiasbynens.be/notes/javascript-unicode#iterating-over-symbols
// prefer for...of iterator; but it is currently not supported by Weixin's
// X5 kernel by Tencent.
function getSymbols(raw) {
    var index = 0;
    var length = raw.length;
    var output = [];
    for (; index < length - 1; ++index) {
        var charCode = raw.charCodeAt(index);
        if (charCode >= 0xD800 && charCode <= 0xDBFF) {
            charCode = raw.charCodeAt(index + 1);
            if (charCode >= 0xDC00 && charCode <= 0xDFFF) {
                output.push(raw.slice(index, index + 2));
                ++index;
                continue;
            }
        }
        output.push(raw.charAt(index));
    }
    output.push(raw.charAt(index));
    return output;
}
function strSlice(raw, end) {
    var s = getSymbols(raw);
    return [
        s.slice(0, end).join(''),
        s.slice(end).join('') // the end and after
    ];
}
function decodedPathname(u) {
    var isDecoded = function (s) {
        return s.indexOf("%") === -1;
    };
    return isDecoded(u) ? u : decodeURIComponent(u);
}
function separatedBy(x, ret, s) {
    if (!ret)
        return s;
    if (!s)
        return ret;
    // ret && s
    return ret + x + s;
}
function px(x) {
    return "" + x + "px";
}
function horizontalExtra(computed, withMargin) {
    var f = (withMargin === undefined ? true : withMargin);
    var x = parseFloat(f ? computed.marginLeft : "0")
        + parseFloat(f ? computed.marginRight : "0")
        + parseFloat(computed.borderLeftWidth)
        + parseFloat(computed.borderRightWidth)
        + parseFloat(computed.paddingLeft)
        + parseFloat(computed.paddingRight);
    return x;
}
function getEnclosingID(t) {
    return t.id ||
        (t.parentElement ? getEnclosingID(t.parentElement) : '');
}
function textFragments(x) {
    var acc = (function () {
        var s = [];
        return {
            fn: function (_, t) { s = s.concat(t); },
            ret: function () { return s; }
        };
    })();
    var niot = "n-i-o-t";
    var empty = [];
    var cs;
    if (x.nodeType === 3) {
        return [x.textContent];
    }
    if (x.classList && x.classList.contains(niot)) {
        return empty;
    }
    if (isIn(['H1', 'H2', 'P'], x.tagName)) {
        return [x.textContent];
    }
    if (x.tagName === "IMG") {
        return [x.alt];
    }
    if (x instanceof HTMLElement) {
        if (window.getComputedStyle(x).display === "none") {
            return empty;
        }
        cs = x.childNodes;
        if (cs.length === 0)
            return empty;
        // cs.length > 0
        iter(cs, acc.fn, textFragments);
        return acc.ret();
    }
    return empty;
}
function backButtonClosure(t) {
    var stack = [];
    var top = function () { return stack[stack.length - 1]; };
    var updateHref = function (id) {
        t.href = '#' + id;
    };
    updateHref("");
    stack = [{ id: "", y: 0 }]; // stack.length === 1
    return {
        element: t,
        peek: top,
        push: function (id, y) {
            var curr = top();
            // only push new id that is different from the top
            if (id !== curr.id) {
                stack.push({ id: id, y: y });
                updateHref(id);
            }
        },
        pop: function () {
            var curr;
            if (stack.length > 1)
                stack.pop(); // stack.length >= 1
            curr = top();
            updateHref(curr.id);
        }
    };
}
function shareButtonClosure(elmt) {
    var name, text, link;
    var separatedByBar = partial(separatedBy, '‖');
    var separatedByNL = partial(separatedBy, '↵');
    var loc2name = function (loc) {
        var dp = decodedPathname(loc.pathname);
        var s = "/", t = ".html";
        dp = dp.endsWith(t) ? dp.slice(0, dp.length - t.length) : dp;
        dp = dp.split(s).pop();
        var uh = loc.hash.slice(1).replace(/\./gi, "%");
        var dh = decodeURIComponent(uh);
        switch (dh) {
            case "the-title":
            case "wxdyh_qrcode":
                // top level item, no need for either pathname or hash
                return "";
            case "ref-to-original":
            case "the-preface":
            case "编0":
                // second level item, no need for hash
                return dp;
            default:
                return [dp, dh].reduce(separatedByBar);
        }
    };
    return {
        element: elmt,
        showAt: function (y) {
            var top = y + window.pageYOffset - 26;
            elmt.style.top = px(top);
            elmt.style.display = "";
        },
        clear: function () {
            text = link = null;
            elmt.style.display = "none";
        },
        setContent: function (elmt, loc) {
            text = textFragments(elmt).reduce(separatedByNL, '');
            link = loc.href;
            name = loc2name(loc);
        },
        getContent: function () {
            var nchars = 64;
            var sliced = strSlice(text, nchars);
            // sliced[0] may end with the separator,
            // allow it, no trimming
            return [
                name,
                sliced[0] + (sliced[1] ? "……" : ""),
                link
            ].reduce(separatedByBar);
        }
    };
}
var tapOn = (function () {
    var tap = (function () {
        var status = 0; //0 - initial; 1 - started; 2 - moved
        var x, y;
        return {
            // Return true if the op is accepted as part of a tap;
            // return false if otherwise.
            "start": function (e, isTouch) {
                if (status === 0) {
                    status = 1;
                    if (!isTouch) {
                        x = e.layerX;
                        y = e.layerY;
                    }
                    return true;
                }
                else
                    return false;
            },
            "move": function (e) {
                if (status === 1) {
                    status = 2;
                    return true;
                }
                else
                    return false;
            },
            "end": function (e, isTouch) {
                if (status === 1) {
                    status = 0;
                    if (!isTouch && (x !== e.layerX || y !== e.layerY)) {
                        return false;
                    }
                    return true;
                }
                else {
                    status = 0;
                    return false;
                }
            }
        };
    }());
    return function (elmt, handler, doPreventDefault) {
        elmt.addEventListener("mousedown", function (e) {
            tap.start(e, false);
        });
        elmt.addEventListener("click", function (e) {
            if (tap.end(e, false))
                handler(e);
        });
        elmt.addEventListener("touchstart", function (e) {
            var id, srcElmt;
            console.log("touchstart", Date.now());
            tap.start(e, true);
            id = getEnclosingID(e.target);
            if (id) {
                srcElmt = document.getElementById(id);
                srcElmt.style.boxShadow = "inset 0 0 0.5em silver";
            }
        });
        elmt.addEventListener("touchmove", function (e) {
            console.log("touchmove", Date.now());
            tap.move(e);
        });
        elmt.addEventListener("touchend", function (e) {
            var id, srcElmt;
            console.log("touchend", Date.now());
            if (tap.end(e, true)) {
                if (doPreventDefault)
                    e.preventDefault();
                handler(e);
            }
            id = getEnclosingID(e.target);
            if (id) {
                srcElmt = document.getElementById(id);
                srcElmt.style.boxShadow = null;
            }
        });
    };
}());
function overlayClosure(elmt, content, docancel, docopy) {
    var computed, textareaWidth;
    tapOn(elmt, function (e) {
        e.stopPropagation();
    }, false);
    tapOn(docancel, function (e) {
        console.log("docancel");
        elmt.style.display = "none";
        content.removeAttribute("readonly");
        e.stopPropagation();
    }, true);
    tapOn(docopy, function (e) {
        content.focus();
        content.select();
        // copy might not work on some browsers, but at least
        // select() makes it easier for user to manually copy
        // the content in textarea
        document.execCommand("copy");
        // set readonly to prevent software keyboard from showing
        // on devices such as phones
        content.setAttribute("readonly", true);
        e.stopPropagation();
    }, true);
    return {
        "element": elmt,
        "setContent": function (s) {
            content.value = s;
        },
        "show": function () {
            elmt.style.display = "block";
            computed = window.getComputedStyle(content);
            textareaWidth =
                parseFloat(window.getComputedStyle(content.parentNode).width)
                    - horizontalExtra(computed, false);
            content.style.width = px(textareaWidth);
            content.style.height = px(43000 / textareaWidth);
        }
    };
}
var backButton, shareButton, overlay;
window.addEventListener("load", function () {
    backButton = backButtonClosure(document.getElementById("back-button"));
    shareButton = shareButtonClosure(document.getElementById("share-button"));
    overlay = overlayClosure(document.getElementById("overlay"), document.getElementById("share-text"), document.getElementById("cancel-overlay"), document.getElementById("do-copy"));
});
function editHashAndScroll(hash, dontAutoScroll) {
    var backToPrevY = function () {
        var y = window.pageYOffset;
        return function () {
            window.scrollTo(0, y);
        };
    }();
    var elmt = document.getElementById(hash.slice(1));
    console.assert(elmt || dontAutoScroll);
    var x = dontAutoScroll ? 0 : (function () {
        var rect = elmt.getBoundingClientRect();
        var h = rect.bottom - rect.top;
        if (rect.top < 0 || h > window.innerHeight)
            return -1;
        // rect.top >= 0 && h <= window.innerHeight
        if (rect.bottom <= window.innerHeight)
            return 0;
        // rect.bottom > window.innerHeight && h <= window.innerHeight
        return 1;
    })();
    window.location.hash = hash;
    console.log(x);
    switch (x) {
        case 0:
            backToPrevY();
            break;
        case -1:
            elmt.scrollIntoView(true);
            break;
        case 1:
            break;
    }
}
var elmtOnTarget = (function () {
    var targetID = null;
    return {
        "update": function (elmt) {
            var id = elmt.id;
            console.assert(id, "element passed here must have an ID");
            if (id === targetID) {
                console.log("elmtOnTarget: clear");
                editHashAndScroll("", true);
                shareButton.clear();
                targetID = null;
            }
            else {
                console.log("elmtOnTarget: set", Date.now());
                editHashAndScroll("#" + id, true);
                shareButton.showAt(elmt.getBoundingClientRect().top);
                shareButton.setContent(elmt, window.location);
                targetID = id;
                console.log("elmtOnTarget: finished", Date.now());
            }
        }
    };
})();
function tapHandler(e) {
    var src = e.target, id = getEnclosingID(src), elmt, hash;
    var isInPageAnchor = function (s) {
        var a = s.split("://", 2);
        return a.length === 1 && a[0][0] === "#";
    };
    console.log("tapped on ", id);
    if (!id)
        return;
    // id is truthy
    elmt = document.getElementById(id);
    if (id === "share-button") {
        overlay.setContent(shareButton.getContent());
        overlay.show();
        return;
    }
    // if the click is originated from an on screen element,
    // prevent page from scrolling after location.hash update
    if (src.tagName !== "A") {
        elmtOnTarget.update(elmt);
        return;
    } // src.tagName === "A"
    hash = src.getAttribute("href");
    if (isInPageAnchor(hash)) {
        e.preventDefault();
        if (id === "back-button") {
            editHashAndScroll(hash, true);
            window.scrollTo(0, backButton.peek().y);
            backButton.pop();
        }
        else {
            backButton.push(id, window.pageYOffset);
            editHashAndScroll(hash, false);
        }
    }
    else {
        // src is an <a> element with an external link
        src.click();
    }
}
tapOn(window, tapHandler, true);
/*
 The following code transforms page for print media.
 Require browsers' support for window.matchMedia("print").
 */
function isFirefox(ua) {
    // See: https://developer.mozilla.org/en-US/docs
    //      /Web/HTTP/Headers/User-Agent/Firefox
    var p = /rv:\d+\.?\d*\) Gecko\/20100101/;
    return p.test(ua);
}
function applyStyles(element, styleMap) {
    var i, s;
    for (s in styleMap) {
        element.style[s] = styleMap[s];
    }
}
function tabulateATriple(a, b, c) {
    var table = document.createElement("TABLE"), tr = document.createElement("TR"), appendCell = function (row, x) {
        var td = document.createElement("TD");
        td.appendChild(x);
        row.appendChild(td);
    };
    appendCell(tr, a);
    appendCell(tr, b);
    appendCell(tr, c);
    table.appendChild(tr);
    return table;
}
var printNumInFirefox = (function () {
    var parent = function (e) {
        return e.parentNode;
    };
    var wrap = function (e, p) {
        var wrapper, ec = e.cloneNode(true), en = e.querySelector(".entry-num"), enl = en.cloneNode(true), enr = en.cloneNode(true), enstyle = {
            "position": "static",
            "right": "auto",
            "width": "100%"
        };
        applyStyles(ec, { "margin-top": 0, "margin-bottom": 0 });
        ec.querySelector(".entry-num").style.display = "none";
        applyStyles(enl, __assign({}, enstyle, { "text-align": "left" }));
        applyStyles(enr, __assign({}, enstyle, { "text-align": "right" }));
        wrapper = tabulateATriple(enl, ec, enr);
        wrapper.className = "entry-wrapper";
        p.replaceChild(wrapper, e);
    };
    var reset = function (wrapper, p) {
        var e = wrapper.querySelector(".entry");
        applyStyles(e, { "margin-top": "", "margin-bottom": "" });
        p.replaceChild(e, wrapper);
    };
    return {
        addTo: function (es) {
            // add a copy for each entry-num element so that
            // the entry-num can be print on both hands of a page
            iter(es, wrap, parent);
        },
        rmFrom: function (es) {
            // remove the inserted entry-num elements
            iter(es, reset, parent);
        }
    };
})();
var printNum = isFirefox(navigator.userAgent) ? printNumInFirefox :
    (function () {
        var firstENum = function (entry) {
            return entry.querySelector(".entry-num");
        };
        var addAnother = function (p, x) {
            if (x)
                p.insertBefore(x.cloneNode(true), x);
        };
        var reset = function (p, x) {
            if (x)
                p.removeChild(x);
        };
        return {
            addTo: function (es) {
                // add a copy for each entry-num element so that
                // the entry-num can be print on both hands of a page
                iter(es, addAnother, firstENum);
            },
            rmFrom: function (es) {
                // remove the inserted entry-num elements
                iter(es, reset, firstENum);
            }
        };
    })();
var qrcodeGenerator = (function () {
    var id = "qrcode", code;
    var insertElmt = function (elmtId) {
        var e = document.createElement("DIV");
        var f = document.getElementById("toc");
        var p = document.getElementsByClassName("entries-container")[0];
        e.id = elmtId;
        p.insertBefore(e, f);
        return e;
    };
    var removeElmt = function (elmtId) {
        var e, p;
        e = document.getElementById(elmtId);
        if (!e)
            return;
        p = document.querySelector(".entries-container");
        p.removeChild(e);
    };
    var decodedURL = function (loc) {
        var origin = "https://读法.com";
        return origin + decodedPathname(loc.pathname);
    };
    return {
        show: function () {
            var e;
            code = qrcode(10, "L");
            code.addData(decodedURL(window.location));
            code.make();
            e = insertElmt(id);
            e.innerHTML = code.createSvgTag();
        },
        clear: function () {
            removeElmt(id);
            code = undefined;
        }
    };
})();
var printHandler = (function () {
    var selector = 'section[class=entry]:not(#the-preface)';
    return {
        before: function () {
            var es = document.querySelectorAll(selector);
            if (es.length === 0)
                return;
            printNum.addTo(es);
            qrcodeGenerator.show();
        },
        after: function () {
            var es = (printNum === printNumInFirefox ?
                document.querySelectorAll('.entry-wrapper') :
                document.querySelectorAll(selector));
            printNum.rmFrom(es);
            qrcodeGenerator.clear();
        }
    };
})();
if ("onbeforeprint" in window && "onafterprint" in window) {
    window.onbeforeprint = printHandler.before;
    window.onafterprint = printHandler.after;
}
else if ("matchMedia" in window) {
    window.matchMedia("print").addListener(function (pe) {
        if (pe.matches)
            printHandler.before();
        else
            printHandler.after();
    });
}
