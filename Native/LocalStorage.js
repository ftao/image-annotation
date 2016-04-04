Elm.Native.LocalStorage = {};
Elm.Native.LocalStorage.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.LocalStorage = localRuntime.Native.LocalStorage || {};
    if (localRuntime.Native.LocalStorage.values)
    {
        return localRuntime.Native.LocalStorage.values;
    }

    var Task = Elm.Native.Task.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);


    function save(data)
    {
        var key = data["_0"];
        var value = data["_1"];
        localStorage.setItem(key, value);
        return Task.succeed(Utils.Tuple0);
        /*
        return Task.asyncFunction(function(callback) {
            localStorage.setItem(key, value);
            return callback(Task.succeed(Utils.Tuple0));
        });
        */
    }


    function load(key) 
    { 
        return Task.succeed(localStorage.getItem(key));
        //return Task.asyncFunction(function(callback) {
        //    return callback(Task.succeed(localStorage.getItem(key)));   
        //})
    }


    return localRuntime.Native.LocalStorage.values = {
        save: save,
        load: load
    };
};
