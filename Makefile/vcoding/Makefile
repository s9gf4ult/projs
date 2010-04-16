AOPT=-acodec copy
VOPT=-vcodec copy
AFMT=-f mpeg
VFMT=-f mpeg
OFMT=-f mpeg
GETFILE=VTS_02_1.VOB
SETFILE=$(GETFILE)_OUT
AUD1=$(GETFILE)_AUD1
AUD2=$(GETFILE)_AUD2
VID=$(GETFILE)_VID


$(AUD1):$(GETFILE)
	ffmpeg -map 0.1 -i $(GETFILE) -vn $(AFMT) $(AOPT) -y $(AUD1)

$(AUD2):$(GETFILE)
	ffmpeg -map 0.2 -i $(GETFILE) -vn $(AFMT) $(AOPT) -y $(AUD2)

$(VID):$(GETFILE)
	ffmpeg -i $(GETFILE) $(VFMT) -an $(VOPT) -y $(VID)

$(SETFILE):$(AUD1) $(AUD2) $(VID)
	ffmpeg -i $(VID)  -i $(AUD1) -i $(AUD2) -vcodec copy -acodec copy $(OFMT) -y $(SETFILE) -newaudio

all:$(SETFILE)