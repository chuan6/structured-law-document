const fs = require('fs');
const dom = require('jsdom');

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
    case ':':
      d = '：'; break;
    case ',':
      d = '，'; break;
    case ';':
      d = '；'; break
    default:
      d = doSkip(c)? '' : c;
      break;
    }
    cv[i] = d;
  }
  return cv.join('');
}

function getReady(href, name) {
  var mainNode = function (w) {
    return w.document.querySelector('.entries-container');
  };
  var inOriginal = withClassPred('not-in-original-text');

  dom.env({
    file: href,
    done: function (err, window) {
      var fromHTML;

      if (err) throw err;

      fromHTML = norm(domText(mainNode(window), inOriginal));

      fs.readFile(
        'generator/resources/' + name + '.txt',
        'utf8',
        function (err, data) {
          var fromTXT, isEqual;

          if (err) console.log(err);

          fromTXT = norm(data);
          isEqual = (fromTXT === fromHTML);
          if (isEqual) {
            console.log('pass', name);
          } else {
            console.error('fail', name);
          }
        }
      );
    }
  });
}

dom.env('index.html', function (err, window) {
  var entries, e, i;

  if (err) throw err;

  entries = window.document.querySelectorAll('.entry a');
  for (i = 0; i < entries.length; i++) {
    e = entries[i];
    getReady(e.getAttribute('href'), e.textContent);
  }
});
