# Audio Equalizer

Now there is the app project equalizer.lpi, that you can open in Lazarus and compile or run. If it is compiled for your system, you can simply run the executable binary file lib/<your_system>/equalizer* to try it without Lazarus. This revision runs from all locations. You can see screenshots of the current low pass filter model in drawing/application. One is the source code section, that shows, how simple you can model any RLC filter after its circuit diagram. TLowPassFilter.Create uses very simple instructions to model a low pass filter. The difference to the band pass filter in the circuit diagram is, that there is an additional resistor RD parrallel to R1 and C1 and missing C2 in the software model. R2 is not necessary in a low pass, and RD helps to shape the frequency response.
I calculated my filter by setting some data by experience and then changing R1, R2, RD and C1 while watching the frequency response after every change.
An extension for the app could be a user interface, that would not require to model something in the code but graphically in the UI. Try your ideas!

