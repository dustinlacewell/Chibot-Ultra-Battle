== CHUB REVOLUTION BETA RELEASE #3 ==
== READ ME FIRST ==

IMPORTANT! PLEASE READ THIS ENTIRE DOCUMENT BEFORE PROCEEDING!!!
(Or for the important stuff, skip down to "HOW TO INSTALL")

== INTRO ==

One day, I was bored out of my mind, and someone brought the topic of ChUB in a AIM to me. I decided to come back and make a new version of ChUB.

== BUT WHY? ==

'Cuz AOL sucks, plus all the currently existing versions of ChUB won't work properly.

== WHAT ABOUT NOAH'S? ==

Don't know, don't care. Whatever Noah does is his own damn business.

== SO WHERE HAVE YOU BEEN? ==

I went to boot camp, got out of boot camp, went through Nuclear Power School, and now I'm stationed on the USS Dwight D. Eisenhower, a nuclear powered aircraft carrier, which is in drydock at Newport News Shipyard. Basically this means I won't be going to sea until 2004 or 2005.

I tinkered around with beta versions of Sonic games, made some MUGEN stuff (http://mugen.elecbyte.com), and hosted boss battles in BOTVGH (http://www.geocities.com/hcuz). The boss battles in BOTVGH reminded me of ChUB, they were RPG-style turn-based battles.

Now I'm back and working on ChUB.

== WHAT'S THE NAVY LIKE? ==

I hate talking about my job. Next question please.

== SO WHAT'S NEW IN THIS VERSION OF CHUB? ==

'Bout time you asked.

First, and foremost, CHUB DOESN'T WORK WITH AOL ANYMORE!! I've changed ChUB to work on mIRC! It's a lot less buggy, since mIRC doesn't balk when you toy around with it using Windows API, plus it's a lot faster. And some servers, such as freedomirc.net, let you flood in channels, which makes ChUB perfect for use.

ChUB's editors have been revised, they work better now, and the Char Editor is fixed (you can actually save/load chars with the regular dialog now).

The engine itself received major bug fixes, mostly dealing with the CPU. It no longer records CPU kills for a character/person's record, and you have the option of wiping out all records.

Many general glitches and bugs were also fixed. Hopefully, I've fixed the bug where if you exit ChUB and load it again, you get a dialog box saying you can't reload it. I've done everything I can to eliminate it, but there's still a possibility you could get it.

A lot of the AOL-only features were removed.

HostBoot and HostSave were removed, because they're pointless now, since we have more control over IRC than AOL chat.

ChUB also supports a ServerBot (ChUBBot1 and ChUBBot2 on FreedomIRC) which will allow you to see all ChUB games going on. Currently, there is no option to disable this, but a privacy option will be added in the future.

Also, SPC support is in! If you're a big fan of SNES music (SPC files) then this option is for you! The midi listbox now also lists SPC's!

Extra Runes are always enabled, so you don't need to do the trick every time you play.

And last, but certainly not least, the command prefix is now ".", instead of "/", since "/" is used for IRC commands. Not everything in the bot has been changed over to "/" yet, so be careful!

For more new stuff, see WHATSNEW.TXT.

== WHAT HASN'T CHANGED? ==

There are probably several glitches still remaining, but that's what you guys are for.

Yoshi's still around.

The actual method of play hasn't changed much.

Pong's still there.

I haven't updated the old C2K guides yet, so they still refer to old versions of C2K. Be wary if you use these guides.

GodMode's still there, but the key has been changed. So far, nobody knows the key except for me.

== SO WHAT DO I HAVE TO DO? ==

All you have to do is:

#1) Tell me if it worked or not. See the message board thread.
#2) Report any bugs you come across.
#3) You may not distribute this file or post it on your website!

If I can get valid evidence that someone's violating #3 above, I'm pulling the file and I won't let anyone test any more ChUB betas until the final release.

== HOW DO I REPORT BUGS? ==

Fill out this handy-dandy form.

YOUR NAME:
COMPUTER PROCESSOR:
COMPUTER RAM:
VERSION OF WINDOWS:
INTERNET SERVICE PROVIDER:
VERSION OF CHUB: Beta #1
DATE BUG OCCURED:
VERSION OF MIRC:
SERVER YOU WERE HOSTING ON:
CHANNEL YOU WERE HOSTING ON:
ANY OTHER NICKS IN THE ROOM AT THE TIME:
DATASET YOU WERE USING:
BATTLE STATUS AS BEST YOU CAN DESCRIBE IT. (i.e., number of players, what characters were being used, etc.):
CHARACTER/MOVE/WEAPON/ITEM/RUNE/ARENA CAUSING THE BUG:
NATURE OF THE BUG:
ERROR MESSAGES (IF ANY):
DID THIS BUG IMPACT THE REMAINDER OF YOUR CHUB SESSION? HOW? (IF chUB crashed then this answer is yes, it crashed your ChUB):
ANY OTHER INFORMATION YOU THINK MIGHT BE VITAL:

It is very important you fill out as much information as possible.

== KNOWN BUGS ==

ChUB may not unload fully when you close it out. If you try to run ChUB again and get an error message, then you are a victim. Rebooting your computer will fix the glitch. If too many instances of ChUb get "stuck" on your machine, your computer may lock up.

SPC Stereo playback is unavailable. This is because it caused a GPF on my computer and forced me to reboot. It will be enabled as soon as I figure out what the problem is.

The results of the match (who won) are not properly displayed. You'll likely get a list of nicks and a bunch of commas.

CPUs can (still) use Super Moves and Fatalities while Quicked.

If "Loop" is unchecked and you're playing an SPC, the SPC will keep looping anyway. Not a major problem since most SPCs are continuous.

== HOW TO INSTALL ==

Remember all that crap I told you in the last beta?

Well, dump it from your brain. I'm going to give you a brand-new installation process that's so easy, a trained monkey could do it.

STEP 1: Load the copy of mIRC you want to run ChUB in.
STEP 2: Connect to the network and join the channel you want to run ChUB in.
STEP 3: Load ChUB.
STEP 4: Click Options.
STEP 5: Click Output.
STEP 6: Under "Output Channel", type in the name of the channel (with the # sign) you're hosting ChUB in.
STEP 7: Click the "Install ChUB into mIRC" button.

Now ChUB is installed into mIRC!

UN-INSTALLATION:

STEP 1: Click the "remote scripts" button on your mIRC toolbar.
STEP 2: If you don't see the "chub.mrc" file, then click View, and click "chub.mrc".
STEP 3: Click File, Unload.

ChUB is now un-installed.

It is recommended that if you installed ChUB the hard way in Beta #1, that you un-install the "chub.ini" file from mIRC and install using the new method.

== CHANGING OUTPUT CHANNELS ==

Simply change the channel name in the Output window. ChUB will automatically tell mIRC to use the new channel to take input from.

== USING MULTIPLE IRC WINDOWS ==

It is not recommended to run more than one copy of mIRC while hosting ChUB -- ChUB can get the windows confused. This will be corrected in a later version.

== OTHER ==

ChUB Resurrection is hosted in the same way ChUB 2000 SE was.

I hope I didn't miss anything. If I did, I'll probably get 20 emails telling me to get my head out of my ass :)

Enjoy ChUB Resurrection!

Kamek

== HOW DO I... ==

Shaddup, I'm going to play Final Fantasy Tactics.