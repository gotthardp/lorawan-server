/*
 * Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
 * All rights reserved.
 * Distributed under the terms of the MIT License. See the LICENSE file.
 */
var myApp = angular.module('myApp', ['ng-admin', 'uiGmapgoogle-maps', 'googlechart', 'ngVis', 'colorpicker.module']);
myApp.config(['NgAdminConfigurationProvider', function (nga) {
    var admin = nga.application('Server Admin').baseApiUrl('/api/');

    var servers = nga.entity('servers')
        .identifier(nga.field('name'));
    var applications = nga.entity('applications')
        .identifier(nga.field('name'));
    var users = nga.entity('users')
        .identifier(nga.field('name'));
    var gateways = nga.entity('gateways')
        .identifier(nga.field('mac'));
    var networks = nga.entity('networks')
        .identifier(nga.field('name'));
    var multicast_channels = nga.entity('multicast_channels')
        .identifier(nga.field('devaddr'));
    var profiles = nga.entity('profiles')
        .identifier(nga.field('name'));
    var devices = nga.entity('devices')
        .identifier(nga.field('deveui'));
    var nodes = nga.entity('nodes')
        .identifier(nga.field('devaddr'));
    var ignored_nodes = nga.entity('ignored_nodes')
        .identifier(nga.field('devaddr'));
    var txframes = nga.entity('txframes')
        .identifier(nga.field('frid'));
    var rxframes = nga.entity('rxframes')
        .identifier(nga.field('frid'))
        .readOnly();
    var connectors = nga.entity('connectors')
        .identifier(nga.field('connid'));
    var handlers = nga.entity('handlers')
        .identifier(nga.field('app'));
    var connections = nga.entity('connections')
        .identifier(nga.field('app'));
    var events = nga.entity('events')
        .identifier(nga.field('evid'));

    role_choices = [
        { value: 'admin', label: 'admin' }
    ];

    adr_choices = [
        { value: 0, label: 'Disabled' },
        { value: 1, label: 'Auto-Adjust' },
        { value: 2, label: 'Maintain' }
    ];

    fcnt_choices = [
        { value: 0, label: 'Strict 16-bit' },
        { value: 1, label: 'Strict 32-bit' },
        { value: 2, label: 'Reset on zero' },
        { value: 3, label: 'Disabled' }
    ];

    txwin_choices = [
        { value: 0, label: 'Auto' },
        { value: 1, label: 'RX1' },
        { value: 2, label: 'RX2' }
    ];

    format_choices = [
        { value: 'raw', label: 'Raw Data' },
        { value: 'json', label: 'JSON' },
        { value: 'www-form', label: 'Web Form' }
    ];

    power_choices = [
        { value: 0, label: 'Max' },
        { value: 1, label: 'Max - 2 dB' },
        { value: 2, label: 'Max - 4 dB' },
        { value: 3, label: 'Max - 6 dB' },
        { value: 4, label: 'Max - 8 dB' },
        { value: 5, label: 'Max - 10 dB' },
        { value: 6, label: 'Max - 12 dB' },
        { value: 7, label: 'Max - 14 dB' },
        { value: 8, label: 'Max - 16 dB' },
        { value: 9, label: 'Max - 18 dB' },
        { value: 10, label: 'Max - 20 dB' }
    ];

    // ---- servers
    servers.listView().fields([
        nga.field('name').isDetailLink(true),
        nga.field('modules.lorawan_server').label('Version'),
        nga.field('memory').label('Free Memory')
            .map(map_memstats_p),
        nga.field('disk').label('Free Disk')
            .map(map_diskstats_p),
        nga.field('health_alerts', 'choices').label('Alerts')
    ])
    .batchActions([]);
    servers.editionView().fields([
        nga.field('health_alerts', 'choices').label('Alerts')
            .editable(false),
        nga.field('name', 'template').label('Performance')
            .template('<sgraph value="value"></sgraph>'),
        nga.field('modules.lorawan_server').label('Version')
            .editable(false),
        nga.field('memory').label('Free Memory')
            .map(map_memstats_p)
            .editable(false),
        nga.field('disk', 'embedded_list').label('Disks')
            .targetFields([
                nga.field('id'),
                nga.field('size_kb'),
                nga.field('percent_used')
            ])
            .editable(false)
    ]);
    servers.editionView().template(editWithTabsTemplate([
        {name:"General", min:0, max:2},
        {name:"Status", min:2, max:5}
    ]));
    // add to the admin application
    admin.addEntity(servers);

    // ---- users
    users.listView().fields([
        nga.field('name').isDetailLink(true),
        nga.field('roles', 'choices').label('Roles')
            .choices(role_choices)
    ]);
    users.creationView().fields([
        nga.field('name')
            .validation({ required: true }),
        nga.field('pass', 'password').label('Password'),
        nga.field('roles', 'choices').label('Roles')
            .choices(role_choices)
            .validation({ required: true })
    ]);
    users.editionView().fields(users.creationView().fields());
    // add to the admin application
    admin.addEntity(users);

    // ---- gateways
    gateways.listView().fields([
        nga.field('mac').label('MAC').isDetailLink(true),
        nga.field('group'),
        nga.field('desc').label('Description'),
        nga.field('ip_address.ip').label('IP Address'),
        nga.field('dwell', 'float').label('Dwell [%]')
            .map(function(value, entry){ return first(value, 'hoursum', 36000); }),
        nga.field('last_alive', 'datetime').label('Last Alive'),
        nga.field('health_decay', 'number').label('Status')
            .template(function(entry){ return healthIndicator(entry.values) })
    ])
    .sortField('health_decay')
    .sortDir('DESC');

    gateways.creationView().fields([
        nga.field('mac').label('MAC')
            .attributes({ placeholder: 'e.g. 0123456789ABCDEF' })
            .transform(function strip(value, entry) {
                return value.replace(/[-:]/g, '')
            })
            .validation({ required: true, pattern: '[A-Fa-f0-9]{2}([-:]?[A-Fa-f0-9]{2}){7}' }),
        nga.field('group'),
        nga.field('tx_rfch', 'number').label('TX Chain')
            .attributes({ placeholder: 'e.g. 0' })
            .validation({ required: true })
            .defaultValue(0),
        nga.field('ant_gain', 'number').label('Antenna Gain (dBi)')
            .attributes({ placeholder: 'e.g. 6' }),
        nga.field('desc').label('Description'),
        nga.field('gpspos', 'template')
            .validation({required: true })
            .label('Location')
            .template('<map location="value"></map>'),
        nga.field('gpsalt', 'number').label('Altitude'),
        // Status
        nga.field('health_alerts', 'choices').label('Alerts')
            .editable(false),
        nga.field('ip_address.ip').label('IP Address')
            .editable(false),
        nga.field('last_alive', 'datetime').label('Last Alive'),
        nga.field('last_report', 'datetime').label('Last Report'),
        nga.field('mac', 'template').label('Network Delay')
            .template('<pgraph value="value"></pgraph>'),
        nga.field('mac', 'template').label('Transmissions')
            .template('<tgraph value="value"></tgraph>')
    ]);
    gateways.creationView().template(createWithTabsTemplate([
        {name:"General", min:0, max:7}
    ]));
    gateways.editionView().fields(gateways.creationView().fields());
    gateways.editionView().template(editWithTabsTemplate([
        {name:"General", min:0, max:7},
        {name:"Status", min:7, max:13}
    ]));
    // add to the admin application
    admin.addEntity(gateways);

    // ---- networks
    networks.listView().fields([
        nga.field('name').isDetailLink(true),
        nga.field('netid').label('NetID'),
        nga.field('subid').label('SubID')
            .map(format_bitstring),
        nga.field('region')
    ]);
    networks.creationView().fields([
        // General
        nga.field('name')
            .validation({ required: true }),
        nga.field('netid').label('NetID')
            .attributes({ placeholder: 'e.g. 0123AB' })
            .validation({ required: true, pattern: '[A-Fa-f0-9]{6}' }),
        nga.field('subid').label('SubID')
            .map(format_bitstring)
            .transform(parse_bitstring)
            .attributes({ placeholder: 'e.g. 0:3' })
            .validation({ pattern: '([A-Fa-f0-9]{2})*:[0-9]+', validator: validate_bitstring }),
        nga.field('region', 'choice')
            .choices([
                { value: 'EU868', label: 'EU 863-870MHz' },
                { value: 'US902', label: 'US 902-928MHz' },
                // Multitech Private Hybrid Mode
                // http://www.multitech.net/developer/software/lora/introduction-to-lora
                { value: 'US902-PR', label: 'US 902-928MHz (Private Hybrid)' },
                { value: 'CN779', label: 'China 779-787MHz' },
                { value: 'EU433', label: 'EU 433MHz' },
                { value: 'AU915', label: 'Australia 915-928MHz' },
                { value: 'CN470', label: 'China 470-510MHz' },
                { value: 'AS923', label: 'Asia 923MHz' },
                { value: 'KR920', label: 'South Korea 920-923MHz' },
                { value: 'IN925', label: 'India 865-867MHz' }
            ])
            .validation({ required: true }),
        nga.field('tx_codr', 'choice').label('Coding Rate')
            .choices([
                { value: '4/5', label: '4/5' },
                { value: '4/6', label: '4/6' },
                { value: '4/7', label: '4/7' },
                { value: '4/8', label: '4/8' }
            ])
            .validation({ required: true })
            .defaultValue('4/5'),
        nga.field('join1_delay', 'number').label('RX1 Join Delay (s)')
            .attributes({ placeholder: 'e.g. 5' })
            .validation({ required: true })
            .defaultValue(5),
        nga.field('join2_delay', 'number').label('RX2 Join Delay (s)')
            .attributes({ placeholder: 'e.g. 6' })
            .validation({ required: true })
            .defaultValue(6),
        nga.field('rx1_delay', 'number').label('RX1 Delay (s)')
            .attributes({ placeholder: 'e.g. 1' })
            .validation({ required: true })
            .defaultValue(1),
        nga.field('rx2_delay', 'number').label('RX2 Delay (s)')
            .attributes({ placeholder: 'e.g. 2' })
            .validation({ required: true })
            .defaultValue(2),
        nga.field('gw_power', 'number').label('Gateway Power (dBm)')
            .attributes({ placeholder: 'e.g. 16' })
            .validation({ required: true }),
        // ADR
        nga.field('max_eirp', 'number').label('Max EIRP (dBm)')
            .attributes({ placeholder: 'e.g. 14' })
            .validation({ required: true }),
        nga.field('max_power', 'choice').label('Max Power')
            .choices(power_choices)
            .validation({ required: true }),
        nga.field('min_power', 'choice').label('Min Power')
            .choices(power_choices)
            .validation({ required: true }),
        nga.field('max_datr', 'choice').label('Max Data Rate')
            .choices(function(entry) {
                if(entry.values.region)
                    return choices_regions[entry.values.region].uplink_datar;
                else
                    return [];
            })
            .validation({ required: true }),
        nga.field('rxwin_init.rx1_dr_offset', 'number').label('Initial RX1 DR Offset')
            .validation({ required: true })
            .defaultValue(0),
        nga.field('rxwin_init.rx2_dr', 'choice').label('Initial RX2 DR')
            .choices(function(entry) {
                if(entry.values.region)
                    return choices_regions[entry.values.region].downlink_datar;
                else
                    return [];
            })
            .validation({ required: true }),
        nga.field('rxwin_init.rx2_freq', 'float').label('Initial RX2 Freq (MHz)')
            .validation({ required: true, validator: validate_frequency }),
        // Channels
        nga.field('init_chans').label('Initial Channels')
            .attributes({ placeholder: 'e.g. 0-2' })
            .validation({ required: true, pattern: '[0-9]+(-[0-9]+)?(,[ ]*[0-9]+(-[0-9]+)?)*' }),
        nga.field('cflist', 'embedded_list').label('Channels')
            .targetFields([
                nga.field('freq', 'float').label('Frequency (MHz)')
                    .validation({ required: true, validator: validate_frequency }),
                nga.field('min_datr', 'number').label('Min Data Rate'),
                nga.field('max_datr', 'number').label('Max Data Rate')
            ])
    ])
    .prepare(['$http', function($http) {
        return $http.get('/api/choices/regions')
            .then(response => { choices_regions = response.data });
    }]);
    networks.creationView().template(createWithTabsTemplate([
        {name:"General", min:0, max:10},
        {name:"ADR", min:10, max:17},
        {name:"Channels", min:17, max:19}
    ]));
    networks.editionView().fields(networks.creationView().fields())
    .prepare(['$http', function($http) {
        return $http.get('/api/choices/regions')
            .then(response => { choices_regions = response.data });
    }]);
    networks.editionView().template(editWithTabsTemplate([
        {name:"General", min:0, max:10},
        {name:"ADR", min:10, max:17},
        {name:"Channels", min:17, max:19}
    ]));
    // add to the admin application
    admin.addEntity(networks);

    // ---- multicast_channels
    multicast_channels.listView().title('Multicast Channels');
    multicast_channels.listView().fields([
        nga.field('devaddr').label('DevAddr').isDetailLink(true),
        nga.field('profiles', 'choices'),
        nga.field('fcntdown', 'number').label('FCnt Down')
    ])
    .sortField('devaddr')
    .sortDir('ASC');

    multicast_channels.creationView().fields([
        nga.field('devaddr').label('DevAddr')
            .attributes({ placeholder: 'e.g. ABC12345' })
            .validation({ required: true, pattern: '[A-Fa-f0-9]{8}' }),
        nga.field('profiles', 'reference_many')
            .targetEntity(profiles)
            .targetField(nga.field('name')),
        nga.field('nwkskey').label('NwkSKey')
            .attributes({ placeholder: 'e.g. FEDCBA9876543210FEDCBA9876543210' })
            .validation({ required: true, pattern: '[A-Fa-f0-9]{32}' }),
        nga.field('appskey').label('AppSKey')
            .attributes({ placeholder: 'e.g. FEDCBA9876543210FEDCBA9876543210' })
            .validation({ required: true, pattern: '[A-Fa-f0-9]{32}' }),
        nga.field('fcntdown', 'number').label('FCnt Down')
            .defaultValue(0)
            .validation({ required: true })
    ]);
    multicast_channels.editionView().fields(multicast_channels.creationView().fields());
    // add to the admin application
    admin.addEntity(multicast_channels);

    // ---- profiles
    profiles.listView().fields([
        nga.field('name').isDetailLink(true),
        nga.field('network'),
        nga.field('app').label('Application'),
        nga.field('appid').label('App Identifier')
    ]);
    profiles.creationView().fields([
        nga.field('name')
            .validation({ required: true }),
        nga.field('network', 'reference')
            .targetEntity(networks)
            .targetField(nga.field('name'))
            .validation({ required: true }),
        nga.field('app', 'reference').label('Application')
            .targetEntity(applications)
            .targetField(nga.field('name'))
            .validation({ required: true }),
        nga.field('appid').label('App Identifier'),
        nga.field('can_join', 'boolean').label('Can Join?')
            .defaultValue(true),
        nga.field('fcnt_check', 'choice').label('FCnt Check')
            .choices(fcnt_choices)
            .defaultValue(0), // Strict 16-bit
        nga.field('txwin', 'choice').label('TX Window')
            .choices(txwin_choices)
            .defaultValue(0), // Auto
        nga.field('adr_mode', 'choice').label('ADR Mode')
            .choices(adr_choices)
            .defaultValue(0),
        nga.field('adr_set.power', 'choice').label('Set Power')
            .choices(function(entry) {
                if(entry.values.network)
                    return choices_networks[entry.values.network].power;
                else
                    return [];
            }),
        nga.field('adr_set.datr', 'choice').label('Set Data Rate')
            .choices(function(entry) {
                if(entry.values.network)
                    return choices_networks[entry.values.network].uplink_datar;
                else
                    return [];
            }),
        nga.field('max_datr', 'choice').label('Max Data Rate')
            .choices(function(entry) {
                if(entry.values.network)
                    return choices_networks[entry.values.network].uplink_datar;
                else
                    return [];
            }),
        nga.field('adr_set.chans').label('Set Channels')
            .attributes({ placeholder: 'e.g. 0-2' })
            .validation({ pattern: '[0-9]+(-[0-9]+)?(,[ ]*[0-9]+(-[0-9]+)?)*' }),

        nga.field('rxwin_set.rx1_dr_offset', 'number').label('Set RX1 DR Offset'),
        nga.field('rxwin_set.rx2_dr', 'choice').label('Set RX2 DR')
            .choices(function(entry) {
                if(entry.values.network)
                    return choices_networks[entry.values.network].downlink_datar;
                else
                    return [];
            }),
        nga.field('rxwin_set.rx2_freq', 'float').label('Set RX2 Freq (MHz)')
            .validation({ validator: validate_frequency }),

        nga.field('request_devstat', 'boolean').label('Request Status?')
            .defaultValue(true)
    ])
    .prepare(['$http', function($http) {
        return $http.get('/api/choices/networks')
            .then(response => { choices_networks = response.data });
    }]);
    profiles.creationView().template(createWithTabsTemplate([
        {name:"General", min:0, max:7},
        {name:"ADR", min:7, max:16}
    ]));
    profiles.editionView().fields(profiles.creationView().fields())
    .prepare(['$http', function($http) {
        return $http.get('/api/choices/networks')
            .then(response => { choices_networks = response.data });
    }]);
    profiles.editionView().template(editWithTabsTemplate([
        {name:"General", min:0, max:7},
        {name:"ADR", min:7, max:16}
    ]));
    // add to the admin application
    admin.addEntity(profiles);

    // ---- devices
    devices.listView().fields([
        nga.field('deveui').label('DevEUI').isDetailLink(true),
        nga.field('profile'),
        nga.field('appargs').label('App Arguments'),
        nga.field('desc').label('Description'),
        nga.field('last_join', 'datetime').label('Last Join'),
        nga.field('node', 'reference')
            .targetEntity(nodes)
            .targetField(nga.field('devaddr'))
    ])
    .sortField('deveui')
    .sortDir('ASC');
    devices.listView().filters([
        nga.field('deveui').label('DevEUI')
            .validation({ pattern: '[A-Fa-f0-9]{16}' }),
        nga.field('profile')
    ]);

    devices.creationView().fields([
        nga.field('deveui').label('DevEUI')
            .attributes({ placeholder: 'e.g. 0123456789ABCDEF' })
            .validation({ required: true, pattern: '[A-Fa-f0-9]{16}' }),
        nga.field('profile', 'reference')
            .targetEntity(profiles)
            .targetField(nga.field('name'))
            .validation({ required: true }),
        nga.field('appargs').label('App Arguments'),
        nga.field('appeui').label('AppEUI')
            .attributes({ placeholder: 'e.g. 0123456789ABCDEF' })
            .validation({ pattern: '[A-Fa-f0-9]{16}' }),
        nga.field('appkey').label('AppKey')
            .attributes({ placeholder: 'e.g. FEDCBA9876543210FEDCBA9876543210' })
            .validation({ required: true, pattern: '[A-Fa-f0-9]{32}' }),
        nga.field('desc').label('Description'),
        nga.field('last_join', 'datetime').label('Last Join'),
        nga.field('node')
            .attributes({ placeholder: 'e.g. ABC12345' })
            .validation({ pattern: '[A-Fa-f0-9]{8}' })
    ]);
    devices.editionView().fields(devices.creationView().fields());
    // add to the admin application
    admin.addEntity(devices);

    // ---- nodes
    nodes.listView().fields([
        nga.field('devaddr').label('DevAddr').isDetailLink(true),
        nga.field('profile', 'reference')
            .targetEntity(profiles)
            .targetField(nga.field('name')),
        nga.field('appargs').label('App Arguments'),
        nga.field('desc').label('Description'),
        nga.field('fcntup', 'number').label('FCnt Up'),
        nga.field('fcntdown', 'number').label('FCnt Down'),
        nga.field('battery', 'number')
            .map(function(value, entry) { return first(entry.devstat, 'battery') }),
        nga.field('margin', 'number').label('D/L SNR')
            .map(function(value, entry) { return first(entry.devstat, 'margin') }),
        nga.field('last_rx', 'datetime').label('Last RX'),
        nga.field('health_decay', 'number').label('Status')
            .template(function(entry){ return healthIndicator(entry.values) })
    ])
    .sortField('health_decay')
    .sortDir('DESC');
    nodes.listView().filters([
        nga.field('devaddr').label('DevAddr')
            .validation({ pattern: '[A-Fa-f0-9]{8}' }),
        nga.field('profile')
    ]);

    nodes.creationView().fields([
        // General
        nga.field('devaddr').label('DevAddr')
            .attributes({ placeholder: 'e.g. ABC12345' })
            .validation({ required: true, pattern: '[A-Fa-f0-9]{8}' }),
        nga.field('profile', 'reference')
            .targetEntity(profiles)
            .targetField(nga.field('name'))
            .validation({ required: true }),
        nga.field('appargs').label('App Arguments'),
        nga.field('nwkskey').label('NwkSKey')
            .attributes({ placeholder: 'e.g. FEDCBA9876543210FEDCBA9876543210' })
            .validation({ required: true, pattern: '[A-Fa-f0-9]{32}' }),
        nga.field('appskey').label('AppSKey')
            .attributes({ placeholder: 'e.g. FEDCBA9876543210FEDCBA9876543210' })
            .validation({ required: true, pattern: '[A-Fa-f0-9]{32}' }),
        nga.field('desc').label('Description'),
        nga.field('fcntup', 'number').label('FCnt Up'),
        nga.field('fcntdown', 'number').label('FCnt Down')
            .defaultValue(0)
            .validation({ required: true })
    ]);
    nodes.creationView().template(createWithTabsTemplate([
        {name:"General", min:0, max:8}
    ]));

    nodes.editionView().fields([
        // General
        nga.field('devaddr').label('DevAddr')
            .attributes({ placeholder: 'e.g. ABC12345' })
            .validation({ required: true, pattern: '[A-Fa-f0-9]{8}' }),
        nga.field('profile', 'reference')
            .targetEntity(profiles)
            .targetField(nga.field('name'))
            .validation({ required: true }),
        nga.field('appargs').label('App Arguments'),
        nga.field('nwkskey').label('NwkSKey')
            .attributes({ placeholder: 'e.g. FEDCBA9876543210FEDCBA9876543210' })
            .validation({ required: true, pattern: '[A-Fa-f0-9]{32}' }),
        nga.field('appskey').label('AppSKey')
            .attributes({ placeholder: 'e.g. FEDCBA9876543210FEDCBA9876543210' })
            .validation({ required: true, pattern: '[A-Fa-f0-9]{32}' }),
        nga.field('desc').label('Description'),
        nga.field('fcntup', 'number').label('FCnt Up'),
        nga.field('fcntdown', 'number').label('FCnt Down')
            .defaultValue(0)
            .validation({ required: true }),
        nga.field('last_reset', 'datetime').label('Last Reset'),
        nga.field('last_rx', 'datetime').label('Last RX'),
        nga.field('device', 'referenced_list')
            .targetEntity(devices)
            .targetReferenceField('node')
            .targetFields([
                nga.field('deveui').label('DevEUI').isDetailLink(true),
                nga.field('last_join', 'datetime')
            ]),
        nga.field('gateways', 'embedded_list')
            .targetFields([
                nga.field('mac').label('MAC'),
                nga.field('rxq.rssi').label('U/L RSSI'),
                nga.field('rxq.lsnr').label('U/L SNR')
            ])
            .editable(false),
        nga.field('downlinks', 'referenced_list')
            .targetEntity(txframes)
            .targetReferenceField('devaddr')
            .targetFields([
                nga.field('datetime', 'datetime').label('Creation Time'),
                nga.field('txdata.port').label('Port'),
                nga.field('txdata.data').label('Data')
            ])
            .listActions(['delete']),
        // ADR
        nga.field('adr_flag', 'choice').label('ADR Support')
            .choices([
                { value: 0, label: 'OFF' },
                { value: 1, label: 'ON' }
            ])
            .editable(false),
        nga.field('adr_set.power', 'choice').label('Set Power')
            .choices(function(entry) {
                if(entry.values.profile)
                    return choices_profiles[entry.values.profile].power;
                else
                    return [];
            }),
        nga.field('adr_set.datr', 'choice').label('Set Data Rate')
            .choices(function(entry) {
                if(entry.values.profile)
                    return choices_profiles[entry.values.profile].uplink_datar;
                else
                    return [];
            }),
        nga.field('adr_set.chans').label('Set Channels')
            .attributes({ placeholder: 'e.g. 0-2' })
            .validation({ pattern: '[0-9]+(-[0-9]+)?(,[ ]*[0-9]+(-[0-9]+)?)*' }),
        nga.field('adr_use.chans').label('Used Channels')
            .editable(false),
        nga.field('adr_failed', 'choices').label('ADR Failed')
            .choices([
                { value: 'power', label: 'power' },
                { value: 'data_rate', label: 'data_rate' },
                { value: 'channel_mask', label: 'channel_mask' }
            ]),

        nga.field('rxwin_use.rx1_dr_offset', 'number').label('Used RX1 DR Offset')
            .editable(false),
        nga.field('rxwin_use.rx2_dr', 'choice').label('Used RX2 DR')
            .choices(function(entry) {
                if(entry.values.profile)
                    return choices_profiles[entry.values.profile].downlink_datar;
                else
                    return [];
            })
            .editable(false),
        nga.field('rxwin_use.rx2_freq', 'float').label('Used RX2 Freq (MHz)')
            .editable(false),

        nga.field('rxwin_failed', 'choices').label('RX Change Failed')
            .choices([
                { value: 'dr_offset', label: 'dr_offset' },
                { value: 'rx2_data_rate', label: 'rx2_data_rate' },
                { value: 'channel', label: 'channel' }
            ]),
        nga.field('devaddr', 'template').label('RX')
            .template('<rgraph value="value"></rgraph>'),
        nga.field('devaddr', 'template').label('RX Quality')
            .template('<qgraph value="value"></qgraph>'),
        // Status
        nga.field('health_alerts', 'choices').label('Alerts')
            .editable(false),
        nga.field('devstat_time', 'datetime').label('Status Time'),
        nga.field('devstat_fcnt', 'number').label('Status FCnt'),
        nga.field('devaddr', 'template').label('Device Status')
            .template('<ngraph value="value"></ngraph>')
    ])
    .prepare(['$http', function($http) {
        return $http.get('/api/choices/profiles')
            .then(response => { choices_profiles = response.data });
    }]);
    nodes.editionView().template(editWithTabsTemplate([
        {name:"General", min:0, max:13},
        {name:"ADR", min:13, max:25},
        {name:"Status", min:25, max:29}
    ]));
    // add to the admin application
    admin.addEntity(nodes);
    admin.addEntity(txframes);

    // ---- ignored nodes
    ignored_nodes.listView().fields([
        nga.field('devaddr').label('DevAddr').isDetailLink(true),
        nga.field('mask')
    ])
    .sortField('devaddr')
    .sortDir('ASC');

    ignored_nodes.creationView().fields([
        nga.field('devaddr').label('DevAddr')
            .attributes({ placeholder: 'e.g. ABC12345' })
            .validation({ required: true, pattern: '[A-Fa-f0-9]{8}' }),
        nga.field('mask')
            .attributes({ placeholder: 'e.g. FFFFFFFF' })
            .validation({ pattern: '[A-Fa-f0-9]{8}' })
    ]);
    ignored_nodes.editionView().fields(ignored_nodes.creationView().fields());
    // add to the admin application
    admin.addEntity(ignored_nodes);

    // ---- rxframes
    rxframes.listView().title('Received Frames')
        .batchActions([]);
    rxframes.listView().fields([
        nga.field('datetime', 'datetime').label('Received'),
        nga.field('app').label('Application'),
        nga.field('devaddr').label('DevAddr')
            .template(function(entry) {
                return "<a href='#/nodes/edit/" + entry.values.devaddr + "'>" + entry.values.devaddr + "</a>";
            }),
        nga.field('mac').label('MAC')
            .map(function(value, entry) {
                return array_slice_mac(entry.gateways);
            })
            .template(function(entry) {
                return format_mac_array(entry.values.mac);
            }),
        nga.field('rssi', 'number').label('U/L RSSI')
            .map(function(value, entry) {
                return array_slice_rxq(entry.gateways, 'rssi');
            })
            .template(function(entry) {
                return entry.values.rssi.join('<br>');
            }),
        nga.field('lsnr', 'number').label('U/L SNR')
            .map(function(value, entry) {
                return array_slice_rxq(entry.gateways, 'lsnr');
            })
            .template(function(entry) {
                return entry.values.lsnr.join('<br>');
            }),
        nga.field('fcnt', 'number').label('FCnt'),
        nga.field('confirm', 'boolean'),
        nga.field('port', 'number'),
        nga.field('data')
            .template(function(entry){
                if(entry.values.data)
                    return "<div title='[ASCII] " + hextoascii(entry.values.data) + "'>" + entry.values.data + "</div>"
            })
    ])
    .sortField('datetime');
    rxframes.listView().filters([
        nga.field('app').label('Application'),
        nga.field('devaddr').label('DevAddr')
            .validation({ pattern: '[A-Fa-f0-9]{8}' })
    ]);
    // add to the admin application
    admin.addEntity(rxframes);

    // ---- connectors
    connectors.listView().fields([
        nga.field('connid').label('Name').isDetailLink(true),
        nga.field('app', 'reference').label('Application')
            .targetEntity(handlers)
            .targetField(nga.field('app')),
        nga.field('uri').label('URI'),
        nga.field('publish_uplinks'),
        nga.field('received').label('Received Topic'),
        nga.field('enabled', 'boolean'),
        nga.field('failed', 'choices')
    ]);
    connectors.creationView().fields([
        nga.field('connid').label('Connector Name')
            .validation({ required: true }),
        nga.field('app', 'reference').label('Application')
            .targetEntity(handlers)
            .targetField(nga.field('app')),
        nga.field('format', 'choice')
            .choices(format_choices)
            .validation({ required: true }),
        nga.field('uri').label('URI')
            .attributes({ placeholder: 'e.g. mqtt://server:8883' })
            .validation({ required: true, pattern: '^(((amqp|mqtt|http)s?:\/\/[^\/?#]+[^?#]*)|ws:|mongodb:\/\/[^\/?#]+)' }),
        nga.field('publish_uplinks'),
        nga.field('publish_events'),
        nga.field('subscribe').label('Subscribe'),
        nga.field('received').label('Received Topic'),
        nga.field('enabled', 'boolean')
            .validation({ required: true }),
        nga.field('failed', 'choices')
            .choices([
                { value: 'badarg', label: 'badarg' },
                { value: 'network', label: 'network' },
                { value: 'topic', label: 'topic' }
            ]),
        // Authentication
        nga.field('client_id').label('Client ID'),
        nga.field('auth', 'choice')
            .choices([
                { value: 'normal', label: 'Username+Password' },
                { value: 'sas', label: 'Shared Access Signature' }
            ]),
        nga.field('name'),
        nga.field('pass').label('Password/Key'),
        nga.field('certfile', 'file').label('User Certificate')
            .uploadInformation({'url': '/api/upload'}),
        nga.field('keyfile', 'file').label('Private Key')
            .uploadInformation({'url': '/api/upload'})
    ]);
    connectors.creationView().template(createWithTabsTemplate([
        {name:"General", min:0, max:10},
        {name:"Authentication", min:10, max:16}
    ]));
    connectors.editionView().fields(connectors.creationView().fields());
    connectors.editionView().template(editWithTabsTemplate([
        {name:"General", min:0, max:10},
        {name:"Authentication", min:10, max:16}
    ]));
    // add to the admin application
    admin.addEntity(connectors);

    // ---- handlers
    handlers.listView().fields([
        nga.field('app').label('Application').isDetailLink(true),
        nga.field('uplink_fields', 'choices'),
        nga.field('payload'),
        nga.field('downlink_expires').label('D/L Expires')
    ]);
    handlers.creationView().fields([
        nga.field('app').label('Application')
            .validation({ required: true }),
        nga.field('uplink_fields', 'choices')
            .choices([
                { value: 'netid', label: 'netid' },
                { value: 'app', label: 'app' },
                { value: 'devaddr', label: 'devaddr' },
                { value: 'deveui', label: 'deveui' },
                { value: 'appargs', label: 'appargs' },
                { value: 'battery', label: 'battery' },
                { value: 'fcnt', label: 'fcnt' },
                { value: 'port', label: 'port' },
                { value: 'data', label: 'data' },
                { value: 'datetime', label: 'datetime' },

                { value: 'freq', label: 'freq' },
                { value: 'datr', label: 'datr' },
                { value: 'codr', label: 'codr' },
                { value: 'best_gw', label: 'best_gw' },
                { value: 'mac', label: 'mac' },
                { value: 'lsnr', label: 'lsnr' },
                { value: 'rssi', label: 'rssi' },
                { value: 'all_gw', label: 'all_gw' }
            ]),
        nga.field('payload', 'choice')
            .choices([
                { value: 'ascii', label: 'ASCII Text' },
                { value: 'cayenne', label: 'Cayenne LPP' }
            ]),
        nga.field('parse_uplink', 'text'),
        nga.field('event_fields', 'choices')
            .choices([
                { value: 'app', label: 'app' },
                { value: 'event', label: 'event' },
                { value: 'devaddr', label: 'devaddr' },
                { value: 'deveui', label: 'deveui' },
                { value: 'appargs', label: 'appargs' },
                { value: 'datetime', label: 'datetime' }
            ]),
        nga.field('parse_event', 'text'),
        nga.field('build', 'text').label('Build Downlink'),
        nga.field('downlink_expires', 'choice').label('D/L Expires')
            .choices([
                { value: 'never', label: 'Never' },
                { value: 'superseded', label: 'When Superseded' }
            ])
            .validation({ required: true }),
        nga.field('app', 'template').label('Test')
            .template('<apptest value="value"></apptest>'),
        nga.field('connectors', 'referenced_list')
            .targetEntity(connectors)
            .targetReferenceField('app')
            .targetFields([
                nga.field('connid').label('Name').isDetailLink(true),
                nga.field('format', 'choice')
                    .choices(format_choices),
                nga.field('uri').label('URI'),
                nga.field('enabled', 'boolean'),
                nga.field('failed', 'choices')
            ])
        ]);
    handlers.editionView().fields(handlers.creationView().fields());
    // add to the admin application
    admin.addEntity(handlers);

    admin.addEntity(connections);

    // ---- events
    events.listView().fields([
        nga.field('severity'),
        nga.field('first_rx', 'datetime').label('First Occurred'),
        nga.field('last_rx', 'datetime').label('Last Occurred'),
        nga.field('count', 'number'),
        nga.field('entity'),
        nga.field('eid')
            .template(function(entry){
                if (entry.values.eid != null) {
                    return "<a href='#/" + entry.values.entity + "s/edit/" + entry.values.eid + "'>" +
                        entry.values.eid + "</a>";
                }
            }),
        nga.field('text', 'wysiwyg'),
        nga.field('args', 'wysiwyg')
    ])
    .sortField('last_rx')
    .listActions('<eventbtn entry="entry"></eventbtn>');
    events.listView().filters([
        nga.field('severity', 'choice')
            .choices([
                { value: 'error', label: 'error' },
                { value: 'warning', label: 'warning' },
                { value: 'info', label: 'info' }
            ]),
        nga.field('entity', 'choice')
            .choices([
                { value: 'server', label: 'server' },
                { value: 'gateway', label: 'gateway' },
                { value: 'device', label: 'device' },
                { value: 'node', label: 'node' }
            ]),
        nga.field('eid')
            .validation({ pattern: '([A-Fa-f0-9]{2})+' }),
        nga.field('text', 'wysiwyg')
    ]);
    // add to the admin application
    admin.addEntity(events);

    // ---- dashboard
    admin.dashboard(nga.dashboard()
        .addCollection(nga.collection(servers)
            .fields([
                nga.field('name').isDetailLink(true),
                nga.field('modules.lorawan_server').label('Version'),
                nga.field('memory').label('Memory')
                    .map(map_memstats),
                nga.field('disk').label('Disk')
                    .map(map_diskstats),
                nga.field('health_decay', 'number').label('Status')
                    .template(function(entry){ return healthIndicator(entry.values) })
            ])
        )
        .addCollection(nga.collection(gateways)
            .fields([
                nga.field('mac').label('MAC').isDetailLink(true),
                nga.field('ip_address.ip').label('IP Address'),
                nga.field('dwell', 'float').label('Dwell [%]')
                    .map(function(value, entry){ return first(value, 'hoursum', 36000); }),
                nga.field('last_alive', 'datetime'),
                nga.field('health_decay', 'number').label('Status')
                    .template(function(entry){ return healthIndicator(entry.values) })
            ])
            .sortField('health_decay')
            .sortDir('DESC')
            .perPage(7)
        )
        .addCollection(nga.collection(nodes)
            .fields([
                nga.field('devaddr').label('DevAddr').isDetailLink(true),
                nga.field('profile', 'reference')
                    .targetEntity(profiles)
                    .targetField(nga.field('name')),
                nga.field('battery', 'number').label('Battery')
                    .map(function(value, entry) { return first(entry.devstat, 'battery') }),
                nga.field('margin', 'number').label('D/L SNR')
                    .map(function(value, entry) { return first(entry.devstat, 'margin') }),
                nga.field('last_rx', 'datetime').label('Last RX'),
                nga.field('health_decay', 'number').label('Status')
                    .template(function(entry){ return healthIndicator(entry.values) })
            ])
            .sortField('health_decay')
            .sortDir('DESC')
            .perPage(7)
        )
        .addCollection(nga.collection(events)
            .fields([
                nga.field('last_rx', 'datetime').label('Last Occurred'),
                nga.field('entity'),
                nga.field('eid')
                    .template(function(entry){
                        if (entry.values.eid != null) {
                            return "<a href='#/" + entry.values.entity + "s/edit/" + entry.values.eid + "'>" +
                                entry.values.eid + "</a>";
                        }
                    }),
                nga.field('text', 'wysiwyg'),
                nga.field('args', 'wysiwyg')
            ])
            .sortField('last_rx')
            .perPage(7)
        )
        .addCollection(nga.collection(rxframes).title('Received Frames')
            .fields([
                nga.field('datetime', 'datetime').label('Received'),
                nga.field('app').label('Application'),
                nga.field('devaddr').label('DevAddr')
                    .template(function(entry) {
                        return "<a href='#/nodes/edit/" + entry.values.devaddr + "'>" + entry.values.devaddr + "</a>";
                    }),
                nga.field('mac').label('MAC')
                    .map(function(value, entry) {
                        return array_slice_mac(entry.gateways);
                    })
                    .template(function(entry) {
                        return format_mac_array(entry.values.mac);
                    }),
                nga.field('lsnr', 'number').label('U/L SNR')
                    .map(function(value, entry) {
                        return array_slice_rxq(entry.gateways, 'lsnr');
                    })
                    .template(function(entry) {
                        return entry.values.lsnr.join('<br>');
                    }),
            ])
            .sortField('datetime')
            .perPage(7)
        )
    );
    var dashLeft = ['servers', 'gateways', 'nodes'];
    var dashRight = ['events', 'rxframes'];

    // ---- menu
    admin.menu(nga.menu()
        .addChild(nga.menu(users).icon('<span class="fa fa-user fa-fw"></span>'))
        .addChild(nga.menu().title('Infrastructure').icon('<span class="fa fa-sitemap fa-fw"></span>')
            .addChild(nga.menu(servers).icon('<span class="fa fa-server fa-fw"></span>'))
            .addChild(nga.menu(gateways).icon('<span class="fa fa-wifi fa-fw"></span>'))
            .addChild(nga.menu(networks).icon('<span class="fa fa-cloud fa-fw"></span>'))
            .addChild(nga.menu(multicast_channels).icon('<span class="fa fa-bullhorn fa-fw"></span>'))
            .addChild(nga.menu(events).icon('<span class="fa fa-exclamation-triangle fa-fw"></span>'))
        )
        .addChild(nga.menu().title('Devices').icon('<span class="fa fa-cubes fa-fw"></span>')
            .addChild(nga.menu(profiles).title('Profiles').icon('<span class="fa fa-pencil-square-o fa-fw"></span>'))
            .addChild(nga.menu(devices).title('Commissioned').icon('<span class="fa fa-cube fa-fw"></span>'))
            .addChild(nga.menu(nodes).title('Activated (Nodes)').icon('<span class="fa fa-rss fa-fw"></span>'))
            .addChild(nga.menu(ignored_nodes).title('Ignored').icon('<span class="fa fa-ban fa-fw"></span>'))
        )
    );
    if (typeof addPrivateMenu === "function") {
        addPrivateMenu(nga, admin, dashLeft, dashRight);
    }
    admin.menu()
        .addChild(nga.menu().title('Backends').icon('<span class="fa fa-industry fa-fw"></span>')
            .addChild(nga.menu(handlers).icon('<span class="fa fa-cogs fa-fw"></span>'))
            .addChild(nga.menu(connectors).icon('<span class="fa fa-bolt fa-fw"></span>'))
        )
        .addChild(nga.menu(rxframes).title('Received Frames').icon('<span class="fa fa-comments fa-fw"></span>'))
        .autoClose(false);

    admin.dashboard()
        .template(dashboardTemplate(dashLeft, dashRight));

    // attach the admin application to the DOM and execute it
    nga.configure(admin);
}]);

function map_memstats(value, entry) {
    return bytesToSize(entry['memory.free_memory']);
}

function map_memstats_p(value, entry) {
    var free = 100 * entry['memory.free_memory'] / entry['memory.total_memory'];
    return bytesToSize(entry['memory.free_memory']) + " (" + free.toFixed(0) + "%)";
}

function map_diskstats(value, entry) {
    var root = entry['disk'].filter(function(obj) {
        return (obj.id === "/");
    });
    if(root.length > 0)
        return bytesToSize(1024*root[0].size_kb * (100-root[0].percent_used)/100);
}

function map_diskstats_p(value, entry) {
    var root = entry['disk'].filter(function(obj) {
        return (obj.id === "/");
    });
    if(root.length > 0)
    {
        var free = 100-root[0].percent_used;
        return bytesToSize(1024*root[0].size_kb * free/100) + " (" + free.toFixed(0) + "%)";
    }
}

function bytesToSize(bytes) {
   var sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB'];
   if (bytes == 0) return '0 Byte';
   var i = parseInt(Math.floor(Math.log(bytes) / Math.log(1024)));
   return Math.round(bytes / Math.pow(1024, i), 2) + ' ' + sizes[i];
}

function array_slice_mac(array) {
    if(Array.isArray(array))
        return array.map( x => x['mac'].toString() );
    else
        return [];
}
function array_slice_rxq(array, slice) {
    if(Array.isArray(array))
        return array.map( x => x['rxq'][slice].toString() );
    else
        return [];
}
function format_mac_array(array) {
    return array.map(mac => '<a href="#/gateways/edit/' + mac + '">' + mac + '</a>' ).join('<br>');
}

function hextoascii(val) {
    var hex  = val.toString();
    var str = '';
    for (var n = 0; n < hex.length; n += 2) {
        var char = parseInt(hex.substr(n, 2), 16);
        if(char == 0x26)
            str += '&amp;';
        else if(char == 0x27)
            str += '&#39;';
        else if (char >= 0x20 && char <= 0x7E)
            str += String.fromCharCode(char);
        else
            str += '.';
    }
    return str;
}

function format_bitstring(value, entry) {
    if(entry["subid.val"] != null)
        return entry["subid.val"] + ":" + entry["subid.len"];
    else
        return null;
}
function validate_bitstring(value) {
    if(value && value.length > 0)
    {
        var parts = value.split(':', 2);
        if(+parts[1] < 0 || +parts[1] > 25)
            throw new Error ('SubID must be less than 25 bits');
    }
}
function parse_bitstring(value, entry) {
    if(value && value.length > 0)
    {
        var parts = value.split(':', 2);
        return {val: parts[0], len: +parts[1]};
    }
    else
        return null;
}

function validate_frequency(value) {
    if(value < 400 || value > 1000)
        throw new Error ('Frequency must be 400..1000 MHz');
}

function enquote(items) {
    return items.map(function(item) { return "'" + item + "'" }).join(',');
}

function first(array, element, divide = 1) {
    if(Array.isArray(array) && array.length > 0)
        return array[0][element] / divide;
}

function dashboardTemplate(leftPanel, rightPanel) {
    var left = enquote(leftPanel);
    var right = enquote(rightPanel);

    return `
<div class="row">
    <div class="col-lg-12">
        <div class="page-header">
            <h1>Dashboard</h1>
        </div>
    </div>
</div>
<div class="row dashboard-content">
    <div class="col-lg-12">
        <div class="panel panel-default">
            <timeline/>
        </div>
    </div>
</div>
<div class="row dashboard-content">
    <div class="col-lg-6">
        <div class="panel panel-default" ng-repeat="name in [${left}]">
            <ma-dashboard-panel collection="dashboardController.collections[name]" entries="dashboardController.entries[name]"
                datastore="dashboardController.datastore"></ma-dashboard-panel>
        </div>
    </div>
    <div class="col-lg-6">
        <div class="panel panel-default" ng-repeat="name in [${right}]">
            <ma-dashboard-panel collection="dashboardController.collections[name]" entries="dashboardController.entries[name]"
                datastore="dashboardController.datastore"></ma-dashboard-panel>
        </div>
    </div>
</div>
    `;
}

function createWithTabsTemplate(list) {
    var R = `
<div class="row">
    <div class="col-lg-12">
        <div class="tab-header">
            <ma-view-actions override="::formController.actions" entry="entry" entity="::formController.entity">
                <ma-list-button ng-if="::entity.listView().enabled" entity="::entity"></ma-list-button>
            </ma-view-actions>
            <h1 compile="::formController.title">
                {{ 'CREATE_NEW' | translate }} {{ ::formController.view.entity.label() | humanize:true | singularize | translate }}
            </h1>
            <p class="lead" ng-if="::formController.description" compile="::formController.description">{{ ::formController.description }}</p>
        </div>
    </div>
</div>
<div class="row" id="create-view" ng-class="::'ng-admin-entity-' + formController.entity.name()">
    <form class="col-lg-12 form-horizontal" name="formController.form" ng-submit="formController.submitCreation($event)">
        <uib-tabset active="active">
    `;
    for(var i = 0; i < list.length; ++i)
    {
        R += '<uib-tab index="' +i+ '" heading="' +list[i].name+ '">'
            + '<div ng-repeat="field in ::formController.fields.slice(' +list[i].min+ ',' +list[i].max+ ' ) track by $index" compile="::field.getTemplateValueWithLabel(entry)">'
            + '<ma-field field="::field" value="entry.values[field.name()]" entry="entry" entity="::entity" form="formController.form" datastore="::formController.dataStore"></ma-field>'
            + '</div>'
            + '</uib-tab>';
    }
    R += `
        </uib-tabset>
        <div class="form-group">
            <div class="col-sm-offset-2 col-sm-10">
                <ma-submit-button label="SUBMIT"></ma-submit-button>
            </div>
        </div>
    </form>
</div>
    `;
    return R;
}

function editWithTabsTemplate(list) {
    var R = `
<div class="row">
    <div class="col-lg-12">
        <div class="tab-header">
            <ma-view-actions override="::formController.actions" entry="entry" entity="::formController.entity">
                <ma-list-button ng-if="::entity.listView().enabled" entity="::entity"></ma-list-button>
                <ma-delete-button ng-if="::entity.deletionView().enabled" entry="entry" entity="::entity"></ma-delete-button>
            </ma-view-actions>
            <h1 compile="::formController.title">
                {{ 'EDIT' | translate }} {{ ::formController.entity.label() | humanize:true | singularize | translate }} #{{ ::entry.identifierValue }}
            </h1>
        </div>
    </div>
</div>
<div class="row" id="edit-view" ng-class="::'ng-admin-entity-' + formController.entity.name()">
    <form class="col-lg-12 form-horizontal" name="formController.form" ng-submit="formController.submitEdition($event)">
        <uib-tabset active="active">
    `;
    for(var i = 0; i < list.length; ++i)
    {
        R += '<uib-tab index="' +i+ '" heading="' +list[i].name+ '">'
            + '<div ng-repeat="field in ::formController.fields.slice(' +list[i].min+ ',' +list[i].max+ ' ) track by $index" compile="::field.getTemplateValueWithLabel(entry)">'
            + '<ma-field field="::field" value="entry.values[field.name()]" entry="entry" entity="::entity" form="formController.form" datastore="::formController.dataStore"></ma-field>'
            + '</div>'
            + '</uib-tab>';
    }
    R += `
        </uib-tabset>
        <div class="form-group">
            <div class="col-sm-offset-2 col-sm-10">
                <ma-submit-button label="SAVE_CHANGES"></ma-submit-button>
            </div>
        </div>
    </form>
</div>
    `;
    return R;
}

function healthIndicator(values) {
    if (values.health_decay != null) {
        if(values.health_decay > 50)
            return '<span style="color:red" class="fa fa-exclamation-circle fa-fw" title="' + values.health_alerts + '"></span>';
        else if(values.health_decay > 0)
            return '<span style="color:orange" class="fa fa-exclamation-triangle fa-fw" title="' + values.health_alerts + '"></span>';
        else
            return '<span style="color:yellowgreen" class="fa fa-check fa-fw" title="ok"></span>';
    }
    else
        return '';
}

myApp.decorator('HttpErrorService', ['$delegate', '$translate', 'notification',
function($delegate, $translate, notification) {
    $delegate.handleDefaultError = function(error) {
        switch (error.status) {
            case 412:
                $delegate.displayError('Data has changed. Please reload and repeat the action.');
                throw error;
            default:
                $translate('STATE_CHANGE_ERROR', { message: error.data.message }).then(this.displayError);
                throw error;
        }
    }
    return $delegate;
}]);

myApp.directive('eventbtn', ['$http', function($http) {
return {
    restrict: 'E',
    scope: {
        entry: '='
    },
    link: function($scope) {
        if($scope.entry.values.entity == "node" && $scope.entry.values.text == "unknown_devaddr")
            $scope.ShowIgnore = true;

        $scope.addIgnored = function() {
            $http({method: 'POST', url: '/api/ignored_nodes', data: {devaddr: $scope.entry.values.eid, mask: 'FFFFFFFF'}});
        }
    },
    template: '<button ng-show="ShowIgnore" ng-click="addIgnored()" type="button" class="btn btn-default btn-xs">ignore</button>'
};}]);

myApp.directive('timeline', ['$http', '$interval', 'VisDataSet', function($http, $interval, VisDataSet) {
return {
    restrict: 'E',
    scope: {
    },
    link: function($scope) {
        $scope.data = {items: VisDataSet([])};
        $scope.options = {
            start: new Date(Date.now() - 600*1000), // 10 minutes ago
            end: new Date(),
            rollingMode: {follow: true, offset: 0.95},
            selectable: false,
            maxHeight: "300px",
            zoomMax: 2592000000,
            zoomMin: 1000
        };
        $scope.events = {
            onload: function(timeline) {
                $scope.timeline = timeline;
                updateData();
            },
            rangechanged: function(event) {
                if(event.byUser)
                    updateData();
            }
        };

        function updateData() {
            var start = new Date($scope.timeline.range.start);
            var end = new Date($scope.timeline.range.end);

            $http({method: 'GET', url: '/admin/timeline',
                    params: {start: start.toISOString(), end: end.toISOString()}})
                .then(function(response) {

                    var newIds = response.data.items.map(function(a) {return a.id;});
                    $scope.data.items.getIds().forEach(function(id) {
                        if(!newIds.includes(id)) $scope.data.items.remove(id);
                    });
                    $scope.data.items.update(response.data.items);
                });
        }
        $scope.stopTime = $interval(updateData, 5000);
        $scope.$on('$destroy', function() {
            $interval.cancel($scope.stopTime);
        });
    },
    template: '<vis-timeline data="data" options="options" events="events"></vis-timeline>'
};}]);

// http://stackoverflow.com/questions/35895411/ng-admin-and-google-maps
myApp.directive('map', [function () {
return {
    restrict: 'E',
    scope: {
        value: '=location',
    },
    link: function($scope, uiGmapIsReady) {
        if ($scope.value == undefined) {
            $scope.value = { lat: 48.88, lon: 14.12};
        }
        $scope.map = { center: { latitude: $scope.value.lat, longitude: $scope.value.lon }, zoom: 4 };
        $scope.marker = {
            id: 0,
            coords: {
                latitude: $scope.value.lat,
                longitude: $scope.value.lon
            },
            options: { draggable: true },
            events: {
                dragend: function (marker, eventName, args) {
                    $scope.value = { lat: marker.getPosition().lat(), lon: marker.getPosition().lng() };
                }
            }
        };
    },
    template:
    `
    <div class="row list-view">
        <div class="col-lg-12">
            <ui-gmap-google-map center="map.center" zoom="map.zoom" draggable="true" options="options" pan=true refresh="true">
                <ui-gmap-marker coords="marker.coords" options="marker.options" events="marker.events" idkey="marker.id"/>
            </ui-gmap-google-map>
        </div>
    </div>
    `
};}]);

myApp.config(function (uiGmapGoogleMapApiProvider) {
    uiGmapGoogleMapApiProvider.configure({
        key: GoogleMapsKey,
        v: '3',
        libraries: 'visualization'
    });
});

myApp.directive('sgraph', ['$http', '$interval', function($http, $interval) {
return {
    restrict: 'E',
    scope: {
        value: '=',
    },
    link: function($scope) {
            function updateData() {
                $http({method: 'GET', url: '/admin/sgraph/'.concat($scope.value)})
                    .then(function(response) {
                        $scope.srvChartObject.data = response.data.array;
                    });
            }
            $scope.srvChartObject = {};
            $scope.srvChartObject.type = "LineChart";
            $scope.srvChartObject.options = {
                "vAxes": {
                    0: {"title": 'Count', "minValue": 0},
                },
                "series": {
                    0: {"targetAxisIndex": 0},
                    1: {"targetAxisIndex": 0}
                },
                "chartArea": {
                    "top": 0, "bottom": "10%",
                    "left": 0, "right": 0
                },
                "focusTarget": "category",
                "legend": {
                    "position": "none"
                },
                "pointSize": 3,
                "hAxis": {
                    "format": 'kk:mm'
                },
                "vAxis": {
                    "textPosition": "in",
                    "gridlines": {"count": -1}
                }
            };
            updateData();
            $scope.stopTime = $interval(updateData, 5000);
            $scope.$on('$destroy', function() {
                $interval.cancel($scope.stopTime);
            });
    },
    template: '<div google-chart chart="srvChartObject"></div>'
};}]);

myApp.directive('pgraph', ['$http', '$interval', function($http, $interval) {
return {
    restrict: 'E',
    scope: {
        value: '=',
    },
    link: function($scope) {
            function updateData() {
                $http({method: 'GET', url: '/admin/pgraph/'.concat($scope.value)})
                    .then(function(response) {
                        $scope.prChartObject.data = response.data.array;
                    });
            }
            $scope.prChartObject = {};
            $scope.prChartObject.type = "LineChart";
            $scope.prChartObject.options = {
                "vAxes": {
                    0: {"title": 'Network Delay [ms]', "minValue": 0, "maxValue": 100},
                },
                "series": {
                    0: {"targetAxisIndex": 0}
                },
                "chartArea": {
                    "top": 0, "bottom": "10%",
                    "left": 0, "right": 0
                },
                "focusTarget": "category",
                "legend": {
                    "position": "none"
                },
                "pointSize": 3,
                "hAxis": {
                    "format": 'kk:mm'
                },
                "vAxis": {
                    "textPosition": "in",
                    "gridlines": {"count": -1}
                }
            };
            updateData();
            $scope.stopTime = $interval(updateData, 5000);
            $scope.$on('$destroy', function() {
                $interval.cancel($scope.stopTime);
            });
    },
    template: '<div google-chart chart="prChartObject"></div>'
};}]);

myApp.directive('tgraph', ['$http', '$interval', function($http, $interval) {
return {
    restrict: 'E',
    scope: {
        value: '=',
    },
    link: function($scope) {
            function updateData() {
                $http({method: 'GET', url: '/admin/tgraph/'.concat($scope.value)})
                    .then(function(response) {
                        $scope.txChartObject.data = response.data.array;
                    });
            }
            $scope.txChartObject = {};
            $scope.txChartObject.type = "LineChart";
            $scope.txChartObject.options = {
                "vAxes": {
                    0: {"title": 'Tx Time [ms]', "minValue": 0, "maxValue": 2000},
                    1: {"title": 'Tx in Hour [ms]', "minValue": 0, "maxValue": 1}
                },
                "series": {
                    0: {"targetAxisIndex": 0, "pointSize": 3},
                    1: {"targetAxisIndex": 1, "pointsVisible": false}
                },
                "chartArea": {
                    "top": 0, "bottom": "10%",
                    "left": 0, "right": 0
                },
                "focusTarget": "category",
                "legend": {
                    "position": "none"
                },
                "interpolateNulls": true,
                "hAxis": {
                    "format": 'kk:mm'
                },
                "vAxis": {
                    "textPosition": "in",
                    "gridlines": {"count": -1}
                }
            };
            updateData();
            $scope.stopTime = $interval(updateData, 5000);
            $scope.$on('$destroy', function() {
                $interval.cancel($scope.stopTime);
            });
    },
    template: '<div google-chart chart="txChartObject"></div>'
};}]);

myApp.directive('rgraph', ['$http', '$interval', function($http, $interval) {
return {
    restrict: 'E',
    scope: {
        value: '=',
    },
    link: function($scope) {
            function updateData() {
                $http({method: 'GET', url: '/admin/rgraph/'.concat($scope.value)})
                    .then(function(response) {
                        $scope.rxChartObject.data = response.data.array;
                        $scope.rxChartObject.options.vAxes[1] = response.data.band;
                    });
            }
            $scope.rxChartObject = {};
            $scope.rxChartObject.type = "LineChart";
            $scope.rxChartObject.options = {
                "vAxes": {
                    0: {"title": 'Data Rate / Power'},
                    1: {"title": 'Frequency (MHz)'}
                },
                "series": {
                    0: {"targetAxisIndex": 0},
                    1: {"targetAxisIndex": 0},
                    2: {"targetAxisIndex": 1}
                },
                "chartArea": {
                    "top": 0, "bottom": "10%",
                    "left": 0, "right": 0
                },
                "focusTarget": "category",
                "legend": {
                    "position": "none"
                },
                "pointSize": 3,
                "vAxis": {
                    "textPosition": "in",
                    "gridlines": {"count": -1}
                },
                "vAxes": {
                    0: {"minValue": 0, "maxValue": 11},
                    1: {"minValue": 433, "maxValue": 928}
                }
            };
            updateData();
            $scope.stopTime = $interval(updateData, 5000);
            $scope.$on('$destroy', function() {
                $interval.cancel($scope.stopTime);
            });
    },
    template: '<div google-chart chart="rxChartObject"></div>'
};}]);

myApp.directive('qgraph', ['$http', '$interval', function($http, $interval) {
return {
    restrict: 'E',
    scope: {
        value: '=',
    },
    link: function($scope) {
            function updateData() {
                $http({method: 'GET', url: '/admin/qgraph/'.concat($scope.value)})
                    .then(function(response) {
                        $scope.rxqChartObject.data = response.data.array;
                    });
            }
            $scope.rxqChartObject = {};
            $scope.rxqChartObject.type = "LineChart";
            $scope.rxqChartObject.options = {
                "vAxes": {
                    0: {"title": 'RSSI (dBm)'},
                    1: {"title": 'U/L SNR (dB)'}
                },
                "series": {
                    0: {"targetAxisIndex": 0, "pointsVisible": false},
                    1: {"targetAxisIndex": 0, "pointSize": 3},
                    2: {"targetAxisIndex": 1, "pointsVisible": false},
                    3: {"targetAxisIndex": 1, "pointSize": 3}
                },
                "chartArea": {
                    "top": 0, "bottom": "10%",
                    "left": 0, "right": 0
                },
                "focusTarget": "category",
                "legend": {
                    "position": "none"
                },
                "vAxis": {
                    "textPosition": "in",
                    "gridlines": {"count": -1}
                },
                "vAxes": {
                    0: {"maxValue": 0},
                    1: {"minValue": 0}
                }
            };
            updateData();
            $scope.stopTime = $interval(updateData, 5000);
            $scope.$on('$destroy', function() {
                $interval.cancel($scope.stopTime);
            });
    },
    template: '<div google-chart chart="rxqChartObject"></div>'
};}]);

myApp.directive('ngraph', ['$http', '$interval', function($http, $interval) {
return {
    restrict: 'E',
    scope: {
        value: '=',
    },
    link: function($scope) {
            function updateData() {
                $http({method: 'GET', url: '/admin/ngraph/'.concat($scope.value)})
                    .then(function(response) {
                        $scope.rxdChartObject.data = response.data.array;
                    });
            }
            $scope.rxdChartObject = {};
            $scope.rxdChartObject.type = "LineChart";
            $scope.rxdChartObject.options = {
                "vAxes": {
                    0: {"title": 'Battery'},
                    1: {"title": 'D/L SNR (dB)'}
                },
                "series": {
                    0: {"targetAxisIndex": 0},
                    1: {"targetAxisIndex": 1},
                    2: {"targetAxisIndex": 1}
                },
                "chartArea": {
                    "top": 0, "bottom": "10%",
                    "left": 0, "right": 0
                },
                "focusTarget": "category",
                "legend": {
                    "position": "none"
                },
                "pointSize": 3,
                "hAxis": {
                    "format": 'M-d'
                },
                "vAxis": {
                    "textPosition": "in",
                    "gridlines": {"count": -1}
                },
                "vAxes": {
                    0: {"minValue":0, "maxValue": 255},
                    1: {"minValue":-32, "maxValue": 31}
                },
                "annotations": {
                    "style": "line"
                }
            };
            updateData();
            $scope.stopTime = $interval(updateData, 5000);
            $scope.$on('$destroy', function() {
                $interval.cancel($scope.stopTime);
            });
    },
    template: '<div google-chart chart="rxdChartObject"></div>'
};}]);

myApp.directive('apptest', ['$http', function($http) {
return {
    restrict: 'E',
    scope: {
        value: '='
    },
    link: function($scope) {
        $scope.count = 0;
        $http({method: 'GET', url: '/api/connections/'+$scope.value})
            .then(function(response) {
                if(response.data.count > 0)
                    $scope.count = response.data.count;
            })

        $scope.sendTest = function() {
            $http({method: 'POST', url: '/api/connections/'+$scope.value+'/send', data: {}});
        }
    },
    template: `
        <button ng-disabled="count <= 0" ng-click="sendTest()" type="button" class="btn btn-default">send</button>
        <ng-pluralize count="count"
            when = "{'0': 'to no connection',
                     'one': 'to 1 connection',
                     'other': 'to {} connections'}">
        </ng-pluralize>
`
};}]);

// end of file
