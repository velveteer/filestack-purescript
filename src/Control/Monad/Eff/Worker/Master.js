var webworkify = require('webworkify');

/**
* Starts a WebWorker from module "Worker"
*
* @param workerModule module that contains "default" function
*/
exports.startWorker = function(workerModule) {
  return function () {
    return webworkify(workerModule);
  }
};

/**
* Sends message to worker.
*
* @param worker
* @param message
*/
exports.sendMessage = function(worker) {
  return function (message) {
    return function () {
      worker.postMessage(message);
    }
  }
};

/**
* Sets up a parent thread to listen for messages from worker
*
* @param unsafePerformEff function to execute monad computation returned by processMessage
* @param worker
* @param cb function that takes message and returns monadic computation of response
*/
exports._onMessage = function (unsafePerformEff) {
  return function(worker) {
    return function (cb) {
      return function () {
        worker.onmessage = function (e) {
          unsafePerformEff(cb(e.data));
        };
      }
    }
  }
};
