AOPT=-acodec aac -ab 128000
VOPT=-vcodec copy
AFMT=-f matroska
VFMT=-f mpeg
OFMT=-f mpeg
GETFILE=VTS_02_1.VOB
SETFILE=$(GETFILE)_OUT
AUD1=$(GETFILE)_AUD1
AUD2=$(GETFILE)_AUD2
VID=$(GETFILE)_VID
VMENFMT=-of lavf -lavfopts format=matroska

$(AUD1):$(GETFILE)
	ffmpeg -map 0.1 -i $(GETFILE) -vn $(AFMT) $(AOPT) -y $(AUD1)

$(AUD2):$(GETFILE)
	ffmpeg -map 0.2 -i $(GETFILE) -vn $(AFMT) $(AOPT) -y $(AUD2)

$(VID):$(GETFILE)
	ffmpeg -i $(GETFILE) $(VFMT) -an $(VOPT) -y $(VID)

$(VID)filter:$(VID)
	mencoder $(VID) $(VMENFMT) -ovc x264 -x264encopts nr=200:bitrate=1000:turbo=1:pass=1 passlogfile=$(VID)log -o /dev/null
	mencoder $(VID) $(VMENFMT) -ovc x264 -x264encopts nr=200:bitrate=1000:turbo=0:pass=3:me=umh:me_range=30:subq=9:trellis=2 passlogfile=$(VID)log -o /dev/null
	mencoder $(VID) $(VMENFMT) -ovc x264 -x264encopts nr=200:bitrate=1000:turbo=0:pass=3:me=umh:me_range=30:subq=9:trellis=2 passlogfile=$(VID)log -o $(VID)filter

$(SETFILE):$(AUD1) $(AUD2) $(VID)filter
	ffmpeg -i $(VID)filter  -i $(AUD1) -i $(AUD2) -vcodec copy -acodec copy $(OFMT) -y $(SETFILE) -newaudio

all:$(SETFILE)
	rm $(AUD1)
	rm $(AUD2)
	rm $(VID)
	rm $(VID)filter