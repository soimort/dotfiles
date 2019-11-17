// ==UserScript==
// @name         Fucking Twitter
// @namespace    http://tampermonkey.net/
// @version      0.1
// @description  do not show unread notifications on Twitter's title
// @author       soimort
// @match        https://twitter.com/*
// @match        https://mobile.twitter.com/*
// @grant        none
// ==/UserScript==

(function() {
  'use strict';

  document.title = "Twitter"; // this is somehow needed (why?)

  var target = document.querySelector('title');

  var observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      //console.log(mutation);
      console.log('Twitter changed title to: "%s"', document.title);

      var m = document.title.match(/^\(\d+\) (.*)$/);
      if (m !== null) {
        var goodTitle = m[1];
        console.log('I will change it back to: "%s"', goodTitle);
        document.title = goodTitle;
      }
    });
  });

  var config = {
    childList: true,
  };
  observer.observe(target, config);
})();
