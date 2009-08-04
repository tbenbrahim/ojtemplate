var texts = new Array();
var states = new Array();

texts['fold000001'] = '<a href="javascript:fold(\'fold000001\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 1 to line 251</i>';
states['fold000001'] = false;
texts['fold000253'] = '<a href="javascript:fold(\'fold000253\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 253 to line 265</i>';
states['fold000253'] = false;
texts['fold000269'] = '<a href="javascript:fold(\'fold000269\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 269 to line 269</i>';
states['fold000269'] = false;
texts['fold000271'] = '<a href="javascript:fold(\'fold000271\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 271 to line 277</i>';
states['fold000271'] = false;
texts['fold000279'] = '<a href="javascript:fold(\'fold000279\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 279 to line 288</i>';
states['fold000279'] = false;
texts['fold000290'] = '<a href="javascript:fold(\'fold000290\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 290 to line 292</i>';
states['fold000290'] = false;
texts['fold000294'] = '<a href="javascript:fold(\'fold000294\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 294 to line 296</i>';
states['fold000294'] = false;
texts['fold000298'] = '<a href="javascript:fold(\'fold000298\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 298 to line 301</i>';
states['fold000298'] = false;
texts['fold000303'] = '<a href="javascript:fold(\'fold000303\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 303 to line 306</i>';
states['fold000303'] = false;
texts['fold000309'] = '<a href="javascript:fold(\'fold000309\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 309 to line 312</i>';
states['fold000309'] = false;
texts['fold000315'] = '<a href="javascript:fold(\'fold000315\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 315 to line 318</i>';
states['fold000315'] = false;
texts['fold000321'] = '<a href="javascript:fold(\'fold000321\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 321 to line 335</i>';
states['fold000321'] = false;
texts['fold000337'] = '<a href="javascript:fold(\'fold000337\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 337 to line 364</i>';
states['fold000337'] = false;
texts['fold000366'] = '<a href="javascript:fold(\'fold000366\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 366 to line 379</i>';
states['fold000366'] = false;
texts['fold000386'] = '<a href="javascript:fold(\'fold000386\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 386 to line 397</i>';
states['fold000386'] = false;
texts['fold000399'] = '<a href="javascript:fold(\'fold000399\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 399 to line 442</i>';
states['fold000399'] = false;
texts['fold000444'] = '<a href="javascript:fold(\'fold000444\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 444 to line 444</i>';
states['fold000444'] = false;
texts['fold000447'] = '<a href="javascript:fold(\'fold000447\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 447 to line 447</i>';
states['fold000447'] = false;
texts['fold000449'] = '<a href="javascript:fold(\'fold000449\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 449 to line 450</i>';
states['fold000449'] = false;
texts['fold000452'] = '<a href="javascript:fold(\'fold000452\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 452 to line 466</i>';
states['fold000452'] = false;
texts['fold000468'] = '<a href="javascript:fold(\'fold000468\');"><img border="0" height="10" width="10" src="plus.png" title="unfold code"/></a><i>&nbsp;&nbsp;code folded from line 468 to line 469</i>';
states['fold000468'] = false;

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
