#### dependencies for Tobii integration
import tobii_research
import os
import numpy as np

from psychopy import visual, event, core, prefs
prefs.general['audioLib'] = ['sounddevice']

from psychopy_tobii_infant import infant_tobii_controller

# CALIBRATION from infanttobiicontroller #
        ###############################################################################
# Constants
DIR = os.path.dirname('C:/Users/CBCD/Desktop/LEAP/PyHab-master/LEAP/stimuli')
# users should know the display well.
DISPSIZE = (1280, 1024)
# define calibration points
CALINORMP = [(0.4, -0.4), (0.4, 0.4), (-0.4, -0.4), (-0.4, 0.4), (0.0, 0.0), (0.0, 0.4), (-0.4, 0.0), (0.0, -0.4), (0.4, 0.0)]
CALIPOINTS = [(x * DISPSIZE[0], y * DISPSIZE[1]) for x, y in CALINORMP]
# correct path for calibration stimuli
CALISTIMS = [
'stimuli/{}'.format(x) for x in os.listdir(os.path.join(DIR, 'stimuli'))
if '.png' in x
]

###############################################################################
# create a Window to control the monitor
win = visual.Window(
    size=[1280, 1024],
    units='pix',
    fullscr=True,
    allowGUI=False)

# initialize tobii_controller to communicate with the eyetracker
controller = infant_tobii_controller(win)
controller.show_status() # show participant eyes within frame

# How to use:
# - Use 1~9 (depending on the number of calibration points) to present
#   calibration stimulus and 0 to hide the target.
# - Press space to start collect calibration samples.
# - Press return (Enter) to finish the calibration and show the result.
# - Choose the points to recalibrate with 1~9.
# - Press decision_key to accept the calibration or recalibrate.
success = controller.run_calibration(CALIPOINTS, CALISTIMS)
if not success:
    core.quit()

# need to stop Tobii calibration timer & restart
# need to close calibration window and open psychopy windows

# Start recording test phase again with PyHab?
controller.start_recording('tobii/demo4-test.csv') # location and name of file containing eye gaze data
waitkey = True
timer = core.Clock()
while waitkey:
    # Get the latest gaze position data.
    currentGazePosition = controller.get_current_gaze_position()

    # The value is numpy.nan if Tobii failed to detect gaze position.
    if np.nan not in currentGazePosition:
        marker.setPos(currentGazePosition)
        marker.setLineColor('white')
    else:
        marker.setLineColor('red')
    keys = event.getKeys()
    if 'space' in keys:
        waitkey = False
    elif len(keys) >= 1:
        # Record the pressed key to the data file.
        controller.record_event(keys[0])
        print('pressed {k} at {t} ms'.format(
            k=keys[0], t=timer.getTime() * 1000))

    marker.draw()
    win.flip()

# stop recording
controller.stop_recording()
# close the file
controller.close()

core.quit()