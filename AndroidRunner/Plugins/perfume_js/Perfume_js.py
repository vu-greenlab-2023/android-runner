import os.path as op
import os
import time
import csv
import threading
import shutil
#http.server import BaseHTTPRequestHandler, HTTPServer
from AndroidRunner.Plugins.perfume_js.server import start_server, stop_server

from AndroidRunner.Plugins.Profiler import Profiler

#https://riptutorial.com/python/example/8570/start-simple-httpserver-in-a-thread-and-open-the-browser
#daemon = 0

class Perfume_js(Profiler): #, BaseHTTPRequestHandler
    def __init__(self, config, paths):
        super(Perfume_js, self).__init__(config, paths)
        self.output_dir = ''
        self.logcat_output = ''
        self.profile = False
        self.metrics = config['metrics']
        #self.httpd= ""

    def start_profiling(self, device, **kwargs):
        self.profile = True
        self.logcat_output = '{}logcat_{}_{}.txt'.format(self.output_dir, device.id, time.strftime('%Y.%m.%d_%H%M%S'))
        #global daemon, stop_daemon
        daemon = threading.Thread(name='daemon_server', target=start_server)	
        daemon.setDaemon(True) # Set as a daemon so it will be killed once the main thread is dead.
        daemon.start()
        time.sleep(1)

    def stop_profiling(self, device, **kwargs):
        self.profile = False
        #global daemon, stop_daemon
        stop_server()

    def collect_results(self, device, path=None):
        perfumeOutputFiles= os.listdir("output")

        for onefile in perfumeOutputFiles:
            #print(any(metr in onefile for metr in self.metrics))
            if any(metr in onefile for metr in self.metrics):
                newFilesDestination= shutil.move("output/"+onefile,self.output_dir)
        shutil.rmtree("output/")

    def set_output(self, output_dir):
        self.output_dir = output_dir

    def dependencies(self):
        return []

    def load(self, device):
        if(os.path.isdir("output")): #deletes folder called 'output' in the root directory (NOTE: The root directory here is the directory where the experiment in executed)
            shutil.rmtree("output/")
        return

    def unload(self, device):
        return

    def aggregate_subject(self):
        return

    def aggregate_end(self, data_dir, output_file):
        return

    def aggregate_final(self, data_dir):
        return

