"use strict";

var SparkMD5 = require('spark-md5');
var btoa = require('abab').btoa;

exports.sparkMD5Impl = function (data) {
  return function () {
    return btoa(SparkMD5.ArrayBuffer.hash(data, true));
  };
};
