A={'Allpass':1, 'Bandpass':1, 'Constant':0, 'Convolver':1, 'Delay':1, 'Dup':3, 'DynamicsCompressor':1, 'Gain':2, 'Highpass':1, 'Highshelf':1, 'LoopBuf':0, 'Lowpass':1, 'Lowshelf':1, 'Microphone':0, 'Notch':1, 'Peaking':1, 'PeriodicOsc':0, 'PlayBuf':0, 'Recorder':1, 'SawtoothOsc':0, 'SinOsc':0, 'Speaker':2, 'SquareOsc':0, 'StereoPanner':1, 'TriangleOsc':0, 'WaveShaper':1}

def _0(s):
  return ''

def _1(s):
  return f''''''

def _2(s):
  return f'''else instance movePointer{s} :: MovePointer' from to (ManyEdges e l) oe => MovePointer at from to (NodeC (T{s} at) (ManyEdges e l)) (NodeC (T{s} at) oe)'''

d=dict(_0=_0,_1=_1,_2=_2,_3=lambda x:'')

for key,value in A.items():
  print(d['_'+str(value)](key))