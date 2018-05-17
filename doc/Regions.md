# Default Regional Configuration
This table suggest values you need to enter in your
[Network](Infrastructure.md) configuration for each region. I strongly recommend you
double-check those with LoRaWAN 1.0.2 Regional Parameters and the registlation in your country.

Parameter              | EU868 | US902 | CN779 | EU433 | AU915 | CN470 | AS923 | KR920 | IN865 | RU868
---------------------- | ----- | ----- | ----- | ----- | ----- | ----- | ----- | ----- | ----- | -----
Coding Rate            | 4/5   | 4/5   | 4/5   | 4/5   | 4/5   | 4/5   | 4/5   | 4/5   | 4/5   | 4/5
RX1 Join Delay (s)     | 5     | 5     | 5     | 5     | 5     | 5     | 5     | 5     | 5     | 5
RX2 Join Delay (s)     | 6     | 6     | 6     | 6     | 6     | 6     | 6     | 6     | 6     | 6
RX1 Delay (s)          | 1     | 1     | 1     | 1     | 1     | 1     | 1     | 1     | 1     | 1
RX2 Delay (s)          | 2     | 2     | 2     | 2     | 2     | 2     | 2     | 2     | 2     | 2
Gateway Power          | 16    | 26    | 12    | 12    | 30    | 19    | 16    | 23    | 30    | 16
Max EIRP (dBm)         | 16    | 30    | 12.15 | 12.15 | 30    | 19.15 | 16    | 14    | 30    | 16
Max Power              | Max   | Max   | Max   | Max   | Max   | Max   | Max   | Max   | Max   | Max
Min Power              | Max - 14dB   | Max - 20dB   | Max - 10dB   | Max - 10dB   | Max - 20dB   | Max - 14dB   | Max - 14dB   | Max - 14dB   | Max - 20dB   | Max - 14dB
Max Data Rate          | SF7 125 kHz  | SF8 500 kHz  | SF7 125 kHz  | SF7 125 kHz  | SF8 500 kHz  | SF7 125 kHz  | SF7 125 kHz  | SF7 125 kHz  | SF7 125 kHz  | SF7 125 kHz
Initial RX1 DR Offset  | 0     | 0     | 0     | 0     | 0     | 0     | 0     | 0     | 0     | 0
Initial RX2 DR         | SF12 125 kHz | SF12 500 kHz | SF12 125 kHz | SF12 125 kHz | SF12 500 kHz | SF12 125 kHz | SF10 125KHz  | SF12 125 kHz | SF10 125 kHz | SF12 125 kHz
Initial RX2 Freq (MHz) | 869.525      | 923.3        | 786          | 434.665      | 923.3        | 505.3        | 923.2        | 921.90       | 866.550      | 869.1
Initial Channels       | 0-2   | 0-71 | 0-2   | 0-2   | 0-71   | 0-95  | 0-x*  | 0-2   | 0-2   | 0-1

In AS923 you need to defined country specific channels. The x* then represents the highest channel you defined.
