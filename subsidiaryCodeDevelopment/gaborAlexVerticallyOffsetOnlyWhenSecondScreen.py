from psychopy import core, visual, event

#If scrn=0, graphics window fills entire screen correctly
#If scrn=1, the graphics window is vertically offset so doesn't draw to entire screen, so you won't see the 'Hit Q to quit' message as it is off screen
scrn=1
myWin = visual.Window(fullscr=1,screen=scrn) 

# INITIALISE SOME STIMULI
gabor = visual.GratingStim(myWin, tex="sin", mask="gauss", texRes=256, 
           size=[1.0, 1.0], sf=[4, 0], ori = 0, name='gabor1')
gabor.autoDraw = True
message = visual.TextStim(myWin, pos=(0.0, -0.9), text='Hit Q to quit')
trialClock = core.Clock()

# repeat drawing for each frame
while trialClock.getTime() < 20: 
    gabor.phase += 0.01
    message.draw()
    # handle key presses each frame
    if event.getKeys(keyList=['escape', 'q']):
        win.close()
        core.quit()

    myWin.flip()

myWin.close()
core.quit()