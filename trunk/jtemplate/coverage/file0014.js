var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 23</i>';
states['fold000001'] = false;
texts['fold000025'] = '<a href="javascript:fold(\'fold000025\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 25 to line 28</i>';
states['fold000025'] = false;
texts['fold000030'] = '<a href="javascript:fold(\'fold000030\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 30 to line 44</i>';
states['fold000030'] = false;

function fold(id) {
  tmp = document.all[id].innerHTML;
  document.all[id].innerHTML = texts[id];
  texts[id] = tmp;
  states[id] = !(states[id]);
}

function unfoldAll() {
  for (key in states) {
    if (states[key]) {
      fold(key);
    }
  }
}

function foldAll() {
  for (key in states) {
    if (!(states[key])) {
      fold(key);
    }
  }
}
