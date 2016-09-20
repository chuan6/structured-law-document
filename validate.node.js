const fs = require('fs');
const dom = require('jsdom');
const utf8 = require('utf8');

function domText(elmt, removeFn) {
    var s = '';
    var cs = elmt.childNodes, i, c;

    for (i = 0; i < cs.length; i++) {
        c = cs[i];

        if (removeFn(c)) continue;

        if (c.nodeType === 3) {
            s += c.textContent;
        } else {
            s += domText(c, removeFn);
        }
    }
    return s;
}

function withClassPred(className) {
    return function (elmt) {
        var flag = false, cs = elmt.classList, i;

        if (!cs) return flag;

        for (i =0; i < cs.length; i++) {
            flag = flag || cs[i] === className;
        }
        return flag;
    };
}

function doSkip(c) {
    return c===' ' || c==='\r' || c==='\n' || c==='\u3000';
}

function norm(s) {
    var cv, c, d, i;

    cv = Array.from(s);
    for (i = 0; i < cv.length; i++) {
        c = cv[i];
        switch (c) {
        case '(':
            d = '（'; break;
        case ')':
            d = '）'; break;
        default:
            d = doSkip(c)? '' : c;
            break;
        }
        cv[i] = d;
    }
    return cv.join('');
}

function getReady(href, name) {
    dom.env({
        file: href,
        done: function (err, window) {
            var fromHTML;

            if (err) console.log(err);

            fromHTML = norm(domText(window.document.querySelector(".entries-container"),
                                    withClassPred('not-in-original-text')));

            fs.readFile(
                'generator/resources/' + name + '.txt',
                'utf8',
                function (err, data) {
                    var fromTXT;

                    if (err) console.log(err);

                    fromTXT = norm(data);
                    console.log(name, fromTXT === fromHTML);
                }
            );
        }
    });
}

dom.env('index.html', function (err, window) {
    var entries, i, name;

    if (err) console.log(err);

    entries = window.document.querySelectorAll('.entry a');
    for (i = 0; i < entries.length; i++) {
        getReady(entries[i].getAttribute('href'), entries[i].textContent);
    }
});
