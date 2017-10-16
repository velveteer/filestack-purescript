"use strict";

var SparkMD5 = require('spark-md5');

exports.sparkMD5Impl = function (data) {
  return window.btoa(SparkMD5.ArrayBuffer.hash(data, true));
};
