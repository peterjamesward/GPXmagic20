//////////////////////////////////////////////////////////////////////
//
// LocalStorage.js
// JavaScript runtime code for Elm LocalStorage module.
// Copyright (c) 2018-2019 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE.txt
//
//////////////////////////////////////////////////////////////////////

// Hideously destructured by PJW.


function storagePortDispatcher(msg) {

    switch (msg.Cmd) {
        case 'get':
            var key = args.key;
            var val = null;
            try {
                val = JSON.parse(storage.getItem(key))
            } catch (e) {
            };
            app.ports.subPort.send(
            { 'msg' : 'got',
                args: { key: key,
                value : val
                 }
            } );
            break;

        case 'put':
            var key = args.key;
            var json = args.value;
            if (typeof(key) == 'string') {
                if (json === null) {
                    storage.removeItem(key);
                } else {
                    var str = JSON.stringify(json);
                    if (typeof(str) == 'string') {
                        storage.setItem(key, str);
                    }
                }
            };
            break;

        case 'listkeys':
            var keys = [];
            var cnt = storage.length;
            for (var i=0; i<cnt; i++) {
                var key = storage.key(i);
                if (key) {
                    keys.push(key);
                }
            };
            app.ports.subPort.send(
            { 'msg' : 'keys',
                args: { keys: keys }
            });
            break;

        case 'clear':
            storage.clear();
            break;

    };
};
