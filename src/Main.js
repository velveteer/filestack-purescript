"use strict";

var SparkMD5 = require('spark-md5');

exports.sparkMD5Impl = function (data) {
  return function () {
    return window.btoa(SparkMD5.ArrayBuffer.hash(data, true));
  };
};
