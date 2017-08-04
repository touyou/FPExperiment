#coding: utf-8

import threading
import socket

from BaseHTTPServer import BaseHTTPRequestHandler
import urlparse

page_head = """
<!doctype html>
<html>
	<head>
		<meta charset="UTF-8">
		<title>othello</title>
		<style type="text/css">
.row {
	display: flex;
	width: 80vw;
	max-width: 80vh;
	height: 10vh;
	max-height: 10vw;
}
.row .block-wrap {
	position: relative;
	display: block;
	width: 12.5%;
	height: 100%;
}
.row .block-wrap .block {
	position: absolute;
	top: 0;
	left: 0;
	border: 1px gray solid;
	box-sizing: border-box;
	width: 100%;
	height: 100%;
}
.block a {
	display:block;
	width: 100%; 
	height: 100%; 
}

.block .circle {
	border-radius: 50%;
	width: 90%;
	height: 90%;
	margin-top: 5%;
	margin-left: 5%;
}

.block .black {
	background: black;
}

.block .white {
	border: 1px gray solid;
	background: white;
}

		</style>
	</head>
	<body>
"""

page_tail = """
	</body>
</html>
"""

def put(bo,y,x,c):
	oc = 3 - c
	if bo[y][x]!=0:
		return False
	#print c,oc,bo
	res = False
	for dy in range(-1,2):
		for dx in range(-1,2):
			if dy == 0 and dx == 0:
				continue
			ok = False
			#print 'try',y,x,':',dy,dx
			ty,tx = -1,-1
			for d in range(1,10):
				ty,tx = y + d * dy, x + d * dx
				if ty < 0 or tx < 0 or ty >= 8 or tx >= 8:
					break
				if d == 1:
					if bo[ty][tx] == oc:
						continue
					break
				else:
					if bo[ty][tx] == oc:
						continue
					if bo[ty][tx] == c:
						ok = True
					break
			
			if ok:
				for d in range(1,10):
					ny,nx = y + d * dy, x + d * dx
					bo[ny][nx] = c
					if ny == ty and nx == tx:
						break
				res = True
	
	if res:
		bo[y][x] = c
	return res

def path2board(pa):
	s = pa[1:]
	res = [[0 for y in xrange(8)] for x in xrange(8)]
	while len(s)>1:
		ns = s[:3]
		s = s[3:]
		y = ord(ns[0])-ord('0')
		x = ord(ns[1])-ord('0')
		t = ord(ns[2])-ord('0')
		res[y][x]=t
	return res

def board2path(bo):
	res = "/"
	for y in xrange(8):
		for x in xrange(8):
			b = bo[y][x]
			if b > 0:
				res += ("%d%d%d" % (y,x,b))
	return res


import copy

def board2page(bo,c):
	res = page_head
	
	wn,bn = 0,0
	
	for y in xrange(8):
		res += '<div class="row">'
		for x in xrange(8):
			res += '<div class="block-wrap"><div class="block">'
			
			b = bo[y][x]
			if b == 1:
				res += '<div class="circle black"></div>'
				bn += 1
			elif b == 2:
				res += '<div class="circle white"></div>'
				wn += 1
			else:
				tb = copy.deepcopy(bo)
				if put(tb,y,x,c):
					res += '<a href="' + board2path(tb) + '"></a>'
			
			res += '</div></div>'
		res += '</div>'
	
	res += '<a href="' + board2path(bo) + '"> PASS </a>'
	res += "    white :: %d ,black :: %d" % (wn,bn)
	res += page_tail
	return res


ccol = -1
ncol = -1

okhttp = False
oktcp = False

nbo = ""


def game_init():
	global ncol,ccol,nbo
	import random
	ccol = 1
	if random.random() > 0.5:
		ccol = 2
	

	nbo = path2board("/332341431442")
	ncol = 1
	

first_http=True

class GetHandler(BaseHTTPRequestHandler):

	def do_GET(self):
		global ncol,ccol,nbo,first_http
		parsed_path = urlparse.urlparse(self.path)
		#print 'path',parsed_path.path
		self.send_response(200)
		self.end_headers()
		
		
		if first_http:
			first_http = False
			if ncol == 3 - ccol: #自分の番
				page = board2page(nbo,3 - ccol)
				self.wfile.write(page)
				return
		else:
			nbo = path2board(parsed_path.path)
			ncol = ccol
		
		while ncol == ccol:
			pass
		
		#cupが打ってnboが更新された
		page = board2page(nbo,3 - ccol)
		self.wfile.write(page)
		



def httpserve():
	global ccol
	from BaseHTTPServer import HTTPServer
	server = HTTPServer(('localhost', 8080), GetHandler)
	server.serve_forever()


def s2pos(s):
	return (ord(s[1])-ord('1'),ord(s[0])-ord('A'))


def boarddiff(bfr,bto):
	res = "PASS"
	for y in xrange(8):
		for x in xrange(8):
			if bfr[y][x]==0 and bto[y][x]>0:
				res = chr(x+ord('A'))+chr(y+ord('1'))
	res = "MOVE " + res + '\n'
	return res

def isfinish(bo):
	cbo = copy.deepcopy(bo)
	for y in xrange(8):
		for x in xrange(8):
			if put(cbo,y,x,1) or put(cbo,y,x,2):
				return False
	return True

def tcpserve():
	global ncol,ccol,nbo
	host = "localhost"
	port = 3000
	serversock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	serversock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
	serversock.bind((host,port))
	serversock.listen(1)
	
	print 'Waiting for connections...'
	clientsock, client_address = serversock.accept()
	rcvmsg = clientsock.recv(1024)
	print 'Received -> %s' % (rcvmsg)
	
	
	ss = 'START %s hoge 1 \n' % ('BLACK' if ccol == 1 else 'WHITE')
	clientsock.send(ss)
	
	
	
	if ccol == 1:
		rs = clientsock.recv(1024)
		rs = rs.split(' ')
		y,x = s2pos(rs[1])
		put(nbo,y,x,ccol)
		clientsock.send('ACK 1 \n')
		ncol = 3 - ccol
	
	mbo = copy.deepcopy(nbo)
	
	while True:
		if ncol == ccol:
			"""
			if isfinish(nbo):
				'END TIE 0 0 huga \n'
			"""
			clientsock.send(boarddiff(mbo,nbo))
			
			rs = clientsock.recv(1024)
			rs = rs.split(' ')
			print rs[1].encode('hex')
			if rs[1] != 'PASS\n':
				y,x = s2pos(rs[1])
				put(nbo,y,x,ccol)
			"""
			if isfinish(nbo):
				'END TIE 0 0 huga \n'
			"""
			clientsock.send('ACK 1 \n')
			mbo = nbo
			ncol = 3 - ccol
		
		
	while True:
		pass
		
		"""
		s = '\n'
		s = 'BYE hoge 1 2 3 \n'
		s = 'ACK 1 \n'
		s = 'OPEN HOGE \n'
		s = 'MOVE PASS \n'
		s = 'END TIE 3 1 huga \n'
		"""

game_init()

tcps = threading.Thread(target=tcpserve, name="")
tcps.daemon = True
tcps.start()

https = threading.Thread(target=httpserve, name="")
https.daemon = True
https.start()


while True:
	raw_input()


