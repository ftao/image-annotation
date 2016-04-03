Elm.Native.ImageAnnotation = {};
Elm.Native.ImageAnnotation.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.ImageAnnotation = localRuntime.Native.ImageAnnotation || {};
    if (localRuntime.Native.ImageAnnotation.values)
    {
        return localRuntime.Native.ImageAnnotation.values;
    }

    var Task = Elm.Native.Task.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);


    function save(data)
    {
        localStorage.setItem("state", data);
        Task.succeed(Utils.Tuple0);
        //return Task.asyncFunction(function(callback) {
        //    return callback(Task.succeed(Utils.Tuple0));
        //});
    }


    function load(key) 
    { 
        return Task.succeed(localStorage.getItem("state"));
        //return Task.asyncFunction(function(callback) {
        //    return callback(Task.succeed(localStorage.getItem("state")));   
        //})
    }


    return localRuntime.Native.ImageAnnotation.values = {
        save: save,
        load: load
    };
};
