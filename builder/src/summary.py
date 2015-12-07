# TODO: continue with infer_good_opts2!!! How different groups combine?
#  It should give an answer to which option is better!
#  It should infer that for some option A1, option B1 may be better but for option A2, option B2 is better.

# TODO: define what is 'better than' (and add other options like: minimize the number of loses, etc., who have more wins, better average speedup, etc.; write advantages and disadvantages of those methods)

import datetime
import string
import sys
import gzip

from operator import itemgetter

defopts = 'highbittags|lowbitgc|qtag|tagswbit|tagtestbit'
defopts_addr = 'qtag'

def opt_groups(opts):
  opts2 = []
  for opt in opts:
    opt2 = opt.split('*')
    opts2.append((len(opt2), opt))
  opts2.sort()
  opts2.reverse()
  opts3 = [opt for (_,opt) in opts2]
  return opts3

# A set of option combinations
class OptGrp:
  def __init__(self, name, list, none, optname):
    self.name = name
    self.list = list
    self.none = none
    self.grps = opt_groups(list) # list of all option combinations
    self.optname = optname

  def splitopts(self, opts):
    opts2=opts.split('|')
    for bigopt in self.grps:
      bigopt2=bigopt.split('*')
      if len(bigopt2) == 1:
        continue # nothing is necessary for combinations of 1 element
      all=True
      for b in bigopt2:
        if b not in opts2:
          all=False
          break
      if all:
        for b in bigopt2:
          opts2.remove(b)
        opts2.append(bigopt)
    return opts2
    
class Summary:
  opts_grp = 'tags'

  # List of option sets that define the different address spaces
  addr_opt=OptGrp(
    "address space options",
    ['qtag',
     'extgc',
     'tagged64*halfwordtags',
     'tagged64*halfwordtags*extgc',
     'pointer64*tagged64',
     'pointer64*tagged64*extgc'
     ],
    'tagged32',
    {'qtag':'addr26',
     'tagged32':'addr27',
     'extgc':'addr29',
     'tagged64*halfwordtags':'addr30',
     'tagged64*halfwordtags*extgc':'addr32',
     'pointer64*tagged64':'addr59',
     'pointer64*tagged64*extgc':'addr61'})
  addr_opt_ptrbits={'qtag':26, # (32-3-1-2)
                    'tagged32':27, # (32-3-2)
                    'extgc':29, # (32-3)
                    'tagged64*halfwordtags':30, # (64-32-2)
                    'tagged64*halfwordtags*extgc':32, # (64-32)
                    'pointer64*tagged64':59, # (64-3-2)
                    'pointer64*tagged64*extgc':61 # (64-3)
                    }
  addr_opt_aligns={'qtag':4,
                   'tagged32':4,
                   'extgc':4,
                   'tagged64*halfwordtags':8,
                   'tagged64*halfwordtags*extgc':8,
                   'pointer64*tagged64':8,
                   'pointer64*tagged64*extgc':8
                   }
  addr_opt_limits={'qtag':256,
                   'tagged32':512,
                   'extgc':2048,
                   'tagged64*halfwordtags':4096,
                   'tagged64*halfwordtags*extgc':4096,
                   'pointer64*tagged64':0, # (in practice, all physical memory)
                   'pointer64*tagged64*extgc':0 # (in practice, all physical memory)
                   }
  
  mem_modes=['totalmem', 'programmem', 'heapmem', 'stackmem', 'trailmem', 'choicemem', 'gc_mem']
  mem_mode_name={
    'totalmem': 'Total',
    'programmem': 'Program',
    'heapmem': 'Heap',
    'stackmem': 'Local',
    'trailmem': 'Trail',
    'choicemem': 'Choice',
    'gc_mem': 'GCed memory'
    }

  time_modes=['without_gc', 'with_gc']
  time_mode_name={
    'with_gc': 'with GC',
    'without_gc': 'without GC'
    }

  bitloc_opt=OptGrp(
    "bit location",
    ['highbittags*lowbitgc',
     'highbittags*highbitgc',
     'highbittags',
     'lowbittags*lowbitgc',
     'lowbittags*highbitgc',
     'lowbittags',
     'splitbittags'],
    '(none)',
    {'highbittags*lowbitgc': 'hightags*lowgc',
     'highbittags*highbitgc': 'hightags*highgc',
     'highbittags': 'hightags',
     'lowbittags*lowbitgc': 'lowtags*lowgc',
     'lowbittags*highbitgc': 'lowtags*highgc',
     'lowbittags': 'lowtags',
     'splitbittags': 'splittags',
     '(none)': '(none)'})
  
  sw_opt=OptGrp(
    "switch method",
    ['tagtestbit','tagswbit*tagtestbit'],
    'cswitch',
    {'tagtestbit': 'sw1',
     'tagswbit*tagtestbit': 'sw2',
     'cswitch': 'sw0'})

  all_opt=OptGrp(
    "bit location and switch method",
    ['highbittags*lowbitgc*tagswbit*tagtestbit',
     'highbittags*highbitgc*tagswbit*tagtestbit',
     'highbittags*tagswbit*tagtestbit',
     'lowbittags*lowbitgc*tagswbit*tagtestbit',
     'lowbittags*highbitgc*tagswbit*tagtestbit',
     'lowbittags*tagswbit*tagtestbit',
     'splitbittags*tagswbit*tagtestbit',
     'highbittags*lowbitgc*tagtestbit',
     'highbittags*highbitgc*tagtestbit',
     'highbittags*tagtestbit',
     'lowbittags*lowbitgc*tagtestbit',
     'lowbittags*highbitgc*tagtestbit',
     'lowbittags*tagtestbit',
     'splitbittags*tagtestbit',
     'highbittags*lowbitgc',
     'highbittags*highbitgc',
     'highbittags',
     'lowbittags*lowbitgc',
     'lowbittags*highbitgc',
     'lowbittags',
     'splitbittags',
     'tagswbit*tagtestbit',
     'tagtestbit'
     ], 'cswitch',
    {'highbittags*lowbitgc*tagswbit*tagtestbit': 'hightags*lowgc*sw2',
     'highbittags*highbitgc*tagswbit*tagtestbit': 'hightags*highgc*sw2',
     'highbittags*tagswbit*tagtestbit': 'hightags*sw2',
     'lowbittags*lowbitgc*tagswbit*tagtestbit': 'lowtags*lowgc*sw2',
     'lowbittags*highbitgc*tagswbit*tagtestbit': 'lowtags*highgc*sw2',
     'lowbittags*tagswbit*tagtestbit': 'lowtags*sw2',
     'splitbittags*tagswbit*tagtestbit': 'splittags*sw2',
     'highbittags*lowbitgc*tagtestbit': 'hightags*lowgc*sw1',
     'highbittags*highbitgc*tagtestbit': 'hightags*highgc*sw1',
     'highbittags*tagtestbit': 'hightags*sw1',
     'lowbittags*lowbitgc*tagtestbit': 'lowtags*lowgc*sw1',
     'lowbittags*highbitgc*tagtestbit': 'lowtags*highgc*sw1',
     'lowbittags*tagtestbit': 'lowtags*sw1',
     'splitbittags*tagtestbit': 'splittags*sw1',
     'highbittags*lowbitgc': 'hightags*lowgc*sw0',
     'highbittags*highbitgc': 'hightags*highgc*sw0',
     'highbittags': 'hightags*sw0',
     'lowbittags*lowbitgc': 'lowtags*lowgc*sw0',
     'lowbittags*highbitgc': 'lowtags*highgc*sw0',
     'lowbittags': 'lowtags*sw0',
     'splitbittags': 'splittags*sw0',
     'tagswbit*tagtestbit': 'sw2',
     'tagtestbit': 'sw1',
     'cswitch': 'sw0'})

  nothing_opt=OptGrp("nothing", [], '(nothing)', {})

  def __init__(self):
    self.names_set={}
    self.names_list=[]
    self.opts_set={}
    self.opts_list=[]
    self.time_set={}
    self.mem_set={}
    self.gc_mem_set={}
    self.gc_time_set={}

    self.speedup_set={}
    self.memgrowth_set={}

    self.report_mem=False
  
  def update_lower(self, set, key, value):
    if (not set.has_key(key)) or set[key] > value:
      set[key] = value
  
  def read_report(self, machine_name, filename):
    self.machine_name = machine_name
    
    if filename.endswith('.gz'):
      openfile = gzip.GzipFile(filename, 'r')
    else:
      openfile = open(filename, 'r')

    programmem = -1
    heapmem = -1
    stackmem = -1
    trailmem = -1
    choicemem = -1

    for line in openfile:
      i = line.find(':')
      if (i == -1):
          continue
      a = line[0:i]
      c = line[i+1:len(line)]
      c = c.strip('\n ')
      c = c.split(' ')
      if (a == 'name'):
        name = c[0]
        if not self.names_set.has_key(name):
          self.names_list += [name]
          self.names_set[name] = True
      elif (a == 'try'):
        trynumber = int(c[0])
      elif (a == 'time'):
        time = float(c[0])
      elif (a == 'program space (including reserved for atoms)'):
        programmem = int(c[0])
      elif (a == 'global stack high water mark'):
        heapmem = int(c[0])
      elif (a == 'local stack high water mark'):
        stackmem = int(c[0])
      elif (a == 'trail stack high water mark'):
        trailmem = int(c[0])
      elif (a == 'control stack high water mark'):
        choicemem = int(c[0])
      elif (a == 'general opts' and self.opts_grp == 'general'):
        opts = c[0]
        if not self.opts_set.has_key(opts):
          self.opts_list += [opts]
          self.opts_set[opts] = True
      elif (a == 'tagscheme opts' and self.opts_grp == 'tags'):
        opts = c[0]
        if not self.opts_set.has_key(opts):
          self.opts_list += [opts]
          self.opts_set[opts] = True
      elif (a == 'garbage collections'):
        # * 1000 because it is in seconds
        gc_mem = int(c[4])
        gc_time = float(c[len(c)-2])*1000
      elif (a == 'total execution time'):
        # last field for the test, update values
        key = name+'-'+opts
        self.update_lower(self.time_set, key, time)
        self.update_lower(self.gc_time_set, key, gc_time)
	# Calculate final memory usage
        if programmem != -1 and heapmem != -1 and stackmem != -1 and trailmem != -1 and choicemem != -1:
          self.report_mem = True
          extgc_factor = self.get_extgc_factor(opts)
          gc_mem = int(gc_mem * extgc_factor)
          heapmem = int(heapmem * extgc_factor)
          stackmem = int(stackmem * extgc_factor)
          trailmem = int(trailmem * extgc_factor)
          choicemem = int(choicemem * extgc_factor)
          totalmem = programmem + heapmem + stackmem + trailmem + choicemem
          # Set memory usage
          self.mem_set[key] = {}
          self.gc_mem_set[key] = gc_mem
          self.mem_set[key]['programmem'] = programmem
          self.mem_set[key]['heapmem'] = heapmem
          self.mem_set[key]['stackmem'] = stackmem
          self.mem_set[key]['trailmem'] = trailmem
          self.mem_set[key]['choicemem'] = choicemem
          self.mem_set[key]['totalmem'] = totalmem
    openfile.close()

  # TODO: include this in statistics, do not patch numbers here
  # Factor to calculate the exact allocated memory (including external
  # gc bits, when they are used)
  def get_extgc_factor(self, opts):
    opts2=opts.split('|')
    if 'tagged64' in opts2:
      tagged_size=8.0
    else:
      tagged_size=4.0
    if 'extgc' in opts2:
      extgc_factor=1.0+1.0/tagged_size
    else:
      extgc_factor=1.0
    return extgc_factor

  def calculate_memgrowth(self, mem_modes):
    # Calculate memory usage
    for mem_mode in self.mem_modes:
      for name in self.names_list:
        for opts in self.opts_list:
          key = name+'-'+opts
          defkey = name+'-'+defopts
          if key not in self.memgrowth_set:
            self.memgrowth_set[key] = {}
          if mem_mode == 'gc_mem':
            if self.gc_mem_set[defkey] == 0:
              memgrowth = 'none'
            else:
              memgrowth = self.gc_mem_set[key]*1.0/self.gc_mem_set[defkey]
            self.memgrowth_set[key][mem_mode] = (memgrowth, self.gc_mem_set[key])
          else:
            if self.mem_set[defkey][mem_mode] == 0:
              memgrowth = 'none'
            else:
              memgrowth = self.mem_set[key][mem_mode]*1.0/self.mem_set[defkey][mem_mode]
            self.memgrowth_set[key][mem_mode] = (memgrowth, self.mem_set[key][mem_mode])
    
  def report_memory_usage(self, detailed):
    if self.report_mem == False:
      print "No memory data collected, omitting memory report."
      return

    self.calculate_memgrowth(self.mem_modes)

    for mem_mode in self.mem_modes:
      print "Memory Report for mode: " + mem_mode

      if detailed == 'full':  
        # Show memory usage
        print "Detailed Memory Usage"
        for name in self.names_list:
          print "%s:" % name
          for opts in self.opts_list:
            key = name+'-'+opts
            (memgrowth, memgrowth_base) = self.memgrowth_set[key][mem_mode]
            if memgrowth == 'none':
              memgrowth_str = "(none)"
            else:
              memgrowth_str = "%.2f" % memgrowth
            print "    %s %.2f %s" %(memgrowth_str, memgrowth_base, opts)
      elif detailed == 'compact':
        # Compact memory usage report
        print "Compact Memory Usage Report"
        for opts in self.opts_list:
          memgrowth_sum = 0
          for name in self.names_list: 
            key = name+'-'+opts
            (memgrowth, _) = self.memgrowth_set[key][mem_mode]
            if memgrowth == 'none':
              pass
            else:
              memgrowth_sum += memgrowth
          opts_memgrowth = memgrowth_sum / len(self.names_list)
          print "    %.2f %s" %(opts_memgrowth, opts)
  
  def report_time(self, detailed):
    for time_mode in self.time_modes:
      print "Time Report for mode: " + time_mode

      # Calculate speedup
      for name in self.names_list:
        for opts in self.opts_list:
          key = name+'-'+opts
          defkey = name+'-'+defopts
          if time_mode == 'without_gc':
            speedup = self.time_set[defkey]*1.0/self.time_set[key]
            speedup_base = self.time_set[key]
          elif time_mode == 'only_gc':
            if self.gc_mem_set[key] == 0:
              speedup = 'none'
            elif self.gc_time_set[key] == 0:
              speedup = 0 # TODO: put undefined instead
            else:
              speedup = self.gc_time_set[defkey]*1.0/self.gc_time_set[key]
            speedup_base = self.gc_time_set[key]
          elif time_mode == 'with_gc':
            speedup = (self.time_set[defkey]+self.gc_time_set[defkey])*1.0/(self.time_set[key]+self.gc_time_set[key])
            speedup_base = self.time_set[key]+self.gc_time_set[key]
          self.speedup_set[key] = (speedup, speedup_base)

      if detailed == 'full':
        # Show speedup per benchmark and options
        print "Name: Speedup (Opts)"
        for name in self.names_list:
          print "%s:" % name
          for opts in self.opts_list:
            key = name+'-'+opts
            (speedup, speedup_base) = self.speedup_set[key]
            if speedup == 'none':
              speedup_str = "(none)"
            else:  
              speedup_str = "%.2f" % speedup
            print "    %s %.2f %s" %(speedup_str, speedup_base, opts)
      elif detailed == 'compact':
        # Show compact report
        print "Wins/loses Speedup-for-winners/Speedup-for-losers Best-speedup/Worse-speedup Overall-speedup Opts"
        for opts in self.opts_list:
          speedup_sum = 0
          speedup_best = 0
          speedup_worse = 1000000
          speedup_win_sum = 0
          speedup_lose_sum = 0
          wins = 0
          loses = 0
          for name in self.names_list: 
            key = name+'-'+opts
            (speedup, _) = self.speedup_set[key]
            if speedup == 'none':
              pass
            else:
              speedup_sum += speedup
              if speedup > speedup_best:
                speedup_best = speedup
              if speedup < speedup_worse:
                speedup_worse = speedup
              if speedup > 1:
                speedup_win_sum += speedup
                wins += 1
              if speedup < 1:
                speedup_lose_sum += speedup
                loses += 1
          if wins != 0:
            opts_speedup_win = speedup_win_sum / wins
          else:
            opts_speedup_win = 0
          if loses != 0:
            opts_speedup_lose = speedup_lose_sum / loses
          else:
            opts_speedup_lose = 0
          opts_speedup = speedup_sum / len(self.names_list)
          print "    %2d / %2d: %.2f/%.2f %.2f/%.2f %.2f %s" %(wins, loses, opts_speedup_win, opts_speedup_lose, speedup_best, speedup_worse, opts_speedup, opts)

  # Obtain the speedup that gives using a single option w.r.t. not using it

  # Note about composition of speed-ups:
  #   Lets ta, tb and tc be the absolute run time of tests a, b and c
  #   so that ta>tb>tc,
  #
  #   The speedup of b w.r.t. a is defined as sba=ta/tb.
  #   The speedup of c w.r.t. b is defined as scb=tb/tc.
  #
  #   Then, the speedup of c w.r.t. a can be obtained by composing the
  #   individual speedup of A and B using the product operand, that is:
  #     sca=scb*sba
  #
  #   Proof: sca=scb*sba=(tb/tc)*(ta/tb)=ta/tc

  def calculate_speedup(self, time_mode):
    # Calculate speedup
    for name in self.names_list:
      for opts in self.opts_list:
        key = name+'-'+opts
        defkey = name+'-'+defopts
        if time_mode == 'without_gc':
          speedup = self.time_set[defkey]*1.0/self.time_set[key]
          speedup_base = self.time_set[key]
        elif time_mode == 'with_gc':
          speedup = (self.time_set[defkey]+self.gc_time_set[defkey])*1.0/(self.time_set[key]+self.gc_time_set[key])
          speedup_base = self.time_set[key]+self.gc_time_set[key]
        self.speedup_set[key] = (speedup, speedup_base)

  def schulze_winners(self, sopts, d_matrix):
    len_sopts = len(sopts)
    winners_list = []
    active_opts = list(xrange(len_sopts))
    while active_opts != []:
      p_matrix = [[0 for y in sopts] for x in sopts]
      for i in active_opts:
        for j in active_opts:
          if i != j:
            if d_matrix[i][j] > d_matrix[j][i]:
              p_matrix[i][j] = d_matrix[i][j]
            else:
              p_matrix[i][j] = 0
      for i in active_opts:
        for j in active_opts:
          if i != j:
            for k in active_opts:
              if i != k:
                if j != k:
                  p_matrix[j][k] = max(p_matrix[j][k], min(p_matrix[j][i], p_matrix[i][k]))
      winner = [True for x in sopts]
      for i in active_opts:
        for j in active_opts:
          if i != j:
            if p_matrix[j][i] > p_matrix[i][j]:
              winner[i] = False
      winners = [sopts[x] for x in active_opts if winner[x]]
      winners_list.append(winners)
      # remove winners from active opts to calculate i-th winners
      active_opts = [x for x in active_opts if not winner[x]]
    return winners_list

  # The absolute winner has the best mean speedup of the first in winners_list
  def absolute_schulze_winner(self, winners_list, speedup_dict):
    pref_list = sorted([(speedup_dict[x], x) for x in winners_list[0]])
    (_,abs_winner) = pref_list[-1]
    return abs_winner

  # The absolute loser has the worst mean speedup of the last in winners_list
  def absolute_schulze_loser(self, winners_list, speedup_dict):
    pref_list = sorted([(speedup_dict[x], x) for x in winners_list[-1]])
    (_,abs_loser) = pref_list[0]
    return abs_loser

  def set_outfile(self, outfile):
    self.outfile = outfile

  def infer_good_opts__classify_opts(self):
    # Classify the initial option lists
    self.copts_list={}
    for opts in self.opts_list:
      opts2 = self.addr_opt.splitopts(opts)
      cinf=self.addr_opt.none
      for x in self.addr_opt.list:
        if x in opts2:
          cinf=x
          break
      if cinf not in self.copts_list:
        self.copts_list[cinf]=[]
      self.copts_list[cinf].append(opts)

    cinfs0 = self.copts_list.keys()
    cinfs0 = [(self.addr_opt_ptrbits[cinf], cinf) for cinf in cinfs0]
    cinfs0.sort()
    self.sorted_cinfs = [cinf for (_,cinf) in cinfs0]

  def infer_good_opts2__detailed_links(self):
    print >>self.outfile, "<p>Results for machine <b>%s</b>:</p>" % (self.machine_name)
    print >>self.outfile, "<div class='normal'>"
    for time_mode in self.time_modes:
      print >>self.outfile, "<a href='infer_%s.html#ref_%s'>Speedup analysis for time <b>%s</b></a><br/>" %(self.machine_name, time_mode, self.time_mode_name[time_mode])
      print >>self.outfile, "<div class='inner'>"
      for cinf in self.sorted_cinfs:
        print >>self.outfile, "<a href='infer_%s.html#ref_%s_%s'>Address space %s</a><br/>" %(self.machine_name, time_mode, cinf, self.addr_opt.optname[cinf])
      print >>self.outfile, "</div>"
    print >>self.outfile, "</div>"

  def infer_good_opts2__memory(self):
    # Process memory usage
    if self.report_mem == False:
      print >>self.outfile, "<p>No memory data collected for machine <b>%s</b>, omitting memory report.</p>" % (self.machine_name)
    else:
      self.calculate_memgrowth(self.mem_modes)
      inf_memgrowth = {}
      for mem_mode in self.mem_modes:
        for cinf in self.sorted_cinfs:
          opts_memgrowth = 0
          for opts in self.copts_list[cinf]:
            memgrowth_sum = 0
            for name in self.names_list: 
              key = name+'-'+opts
              (memgrowth, _) = self.memgrowth_set[key][mem_mode]
              if memgrowth == 'none':
                memgrowth_sum += 1.0
              else:
                memgrowth_sum += memgrowth
            opts_memgrowth0 = memgrowth_sum / len(self.names_list)
            if opts_memgrowth == 0:
              opts_memgrowth = opts_memgrowth0
            elif opts_memgrowth != opts_memgrowth0:
              print >>self.outfile, "<p>Warning: memory growth for address space %s is not homogeneous (at opts %s)</p>" %(self.addr_opt.optname[cinf], opts)
            else:
              pass
          if cinf not in inf_memgrowth:
            inf_memgrowth[cinf] = {}
          inf_memgrowth[cinf][mem_mode] = opts_memgrowth

      print >>self.outfile, "<p>Memory growth for the different memory regions for machine <b>%s</b>:</p>" %(self.machine_name)
      print >>self.outfile, "<table>"
      str = "<td class='empty_top_left' colspan='4'>&nbsp</td>"
      for mem_mode in self.mem_modes:
        str += "<th colspan='3'>%s</th>" %(self.mem_mode_name[mem_mode])
      print >>self.outfile, "<tr>%s</tr>" %(str)
      str = "<th>Address space</th>"
      str += "<th>ptrbits</th>"
      str += "<th>align</th>"
      str += "<th>limits</th>"
      for mem_mode in self.mem_modes:
        str += "<th>ratio</th><th>eff limits</th><th>efflimit ratio</th>" # memory growth and effective memory limits (limits divided by the growth: it makes sense when talking about tagged cells, not for blobs or big numbers!)
      print >>self.outfile, "<tr>%s</tr>" %(str)
      rep=[]
      for cinf in self.sorted_cinfs:
        str = "<td>%s</td>" %(self.addr_opt.optname[cinf])
        str += "<td>%s</td>" %(self.addr_opt_ptrbits[cinf])
        str += "<td>%s</td>" %(self.addr_opt_aligns[cinf])
        if self.addr_opt_limits[cinf] == 0:
          str += "<td>full</td>"
        else:
          str += "<td>%d</td>" %(self.addr_opt_limits[cinf])
        for mem_mode in self.mem_modes:
          str += "<td>%.2f</td>" %(inf_memgrowth[cinf][mem_mode])
          if self.addr_opt_limits[cinf] == 0:
            str += "<td>full</td>"
            str += "<td>-</td>"
          else:
            eff_limit = self.addr_opt_limits[cinf]/inf_memgrowth[cinf][mem_mode]
            def_eff_limit = self.addr_opt_limits[defopts_addr]/inf_memgrowth[defopts_addr][mem_mode]
            str += "<td>%.2f</td>" %(eff_limit)
            str += "<td>%.2f</td>" %(eff_limit / def_eff_limit)
        rep.append(str)
      for x in rep:
        print >>self.outfile, "<tr>%s</tr>" %(x)
      print >>self.outfile, "</table>"

  def infer_good_opts2__detailed(self):
    # Process memory usage
    print >>self.outfile, "<h1>Speedups analysis</h1>"

    self.summary_str=dict([(x, dict([(y, []) for y in self.sorted_cinfs])) for x in self.time_modes])
    self.sabs_summary_str=dict([(x, dict([(y, []) for y in self.sorted_cinfs])) for x in self.time_modes])

    # Process speedups
    for time_mode in self.time_modes:
      print >>self.outfile, "<a id='ref_%s'></a>" %(time_mode)
      print >>self.outfile, "<h2>Time %s</h2>" %(self.time_mode_name[time_mode])
      self.calculate_speedup(time_mode)
      for cinf in self.sorted_cinfs:
        print >>self.outfile, "<a id='ref_%s_%s'></a>" %(time_mode, cinf)
        print >>self.outfile, "<h3>Address space %s:</h3>" %(self.addr_opt.optname[cinf])
        print >>self.outfile, "<div class='inner'>"
  #      print >>self.outfile, "FIXME: it does not make sense doing the mean of different options! only different benchmarks!!"
        projlist = [
          (self.all_opt, self.nothing_opt),
          (self.bitloc_opt, self.sw_opt),
          (self.sw_opt, self.bitloc_opt)
          ]
        for (inf_opt, pinf_opt) in projlist:
          print >>self.outfile, "<div class='inner'>"
          # Classify the projected option lists
          popts_list={}
          for opts in self.copts_list[cinf]:
            opts2 = pinf_opt.splitopts(opts)
            pinf=pinf_opt.none
            for x in pinf_opt.list:
              if x in opts2:
                pinf=x
                break
            if pinf not in popts_list:
              popts_list[pinf]=[]
            popts_list[pinf].append(opts)

          for pinf in popts_list:
    #        print >>self.outfile, "Analyzing the option group %s" %(inf_opt.list)

            lst3={}
            for name in self.names_list:
    #      print >>self.outfile, "Name: %s" %(name)
    #          if not name == 'boyer':
    #            continue # ignore it, extrange case
              lst=[]
              for opts in popts_list[pinf]:
                key = name+'-'+opts
                opts2 = inf_opt.splitopts(opts)
    #            if 'qtag' in opts2:
    #              continue # ignore it 
                (speedup, _) = self.speedup_set[key]
                lst.append({'speedup':speedup, 'opts':opts2})
              lst.sort(key=itemgetter('speedup'))

              lst2={}
              for x in lst:
                speedup=x['speedup']
                opts2=x['opts']
                inf=inf_opt.none
                opts3=list(opts2)
                for inf0 in inf_opt.list:
                  if inf0 in opts2:
                    inf=inf0
                    opts3.remove(inf)
                    break
                key='|'.join(opts3)
                if key not in lst2:
                  lst2[key]={'opts':opts3, 'selopts':[]}
                lst2[key]['selopts'].append({'speedup':speedup, 'inf':inf})
              for key in lst2:
                lst2[key]['selopts'].sort(key=itemgetter('speedup'))

              for key in lst2:
                opts2=lst2[key]['opts']
                selopts=lst2[key]['selopts']
                selopts0=[x['speedup'] for x in selopts]
                selopts1=[x['inf'] for x in selopts]
                selopts3=dict([(x['inf'],x['speedup']) for x in selopts])
                betterinf=selopts1[-1]
                key3b=list(selopts1)
                key3b.sort()
                key3='|'.join(key3b)
                if key3 not in lst3:
                  lst3[key3]={}
                  lst3[key3]['count']=0
                  lst3[key3]['score']=dict([(x,0) for x in selopts1])
                  #lst3[key3]['ranklist']=dict([(x,[0]*len(selopts1)) for x in selopts1])
                  lst3[key3]['classspeedup_mean']=dict([(x,0) for x in selopts1])
                  lst3[key3]['classspeedup_max']=dict([(x,0) for x in selopts1])
                  lst3[key3]['classspeedup_min']=dict([(x,100000) for x in selopts1])
                  lst3[key3]['classspeedup_all']=dict([(x,[]) for x in selopts1])
                  lst3[key3]['classspeedup_count']=0
                lst3[key3]['count']+=1
                lst3[key3]['classspeedup_count']+=1
                # Update score (more wins)
#                lst3[key3]['score'][betterinf]+=1
                # Update score (todo: voting method?)
                # (the final score is the rank in base 'number of cases')
                #score_base=len(selopts1)
                #score_acc=1
                #for i in selopts1:
                #  lst3[key3]['score'][i]+=score_acc
                #  score_acc*=score_base
                # Update rank list
                #for i in xrange(len(selopts1)):
                #  lst3[key3]['ranklist'][selopts1[i]][i]+=1
                for x in selopts3:
                  lst3[key3]['classspeedup_mean'][x]+=selopts3[x]
                  lst3[key3]['classspeedup_max'][x]=max(lst3[key3]['classspeedup_max'][x], selopts3[x])
                  lst3[key3]['classspeedup_min'][x]=min(lst3[key3]['classspeedup_min'][x], selopts3[x])
                  lst3[key3]['classspeedup_all'][x].append((selopts3[x],name))

    #            print >>self.outfile, "opts: %s, spd %s, selopts %s" %(opts2, selopts0, selopts1)

            for key3 in lst3:
              for x in lst3[key3]['classspeedup_mean']:
                lst3[key3]['classspeedup_mean'][x]/=lst3[key3]['classspeedup_count']
            for key3 in lst3:
              for x in lst3[key3]['classspeedup_all']:
                lst3[key3]['classspeedup_all'][x].sort()
            #for key3 in lst3:
            #  for x in lst3[key3]['ranklist']:
            #    lst3[key3]['ranklist'][x].reverse()

            print >>self.outfile, "<h4>Best %s for the %s %s:</h4>" %(inf_opt.name, pinf_opt.name, pinf)
            print >>self.outfile, "<div class='inner'>"
            for key3 in lst3:
              # Calculate tblnames and test_speedup
              test_speedup = {}
              tblnames={}
              sopts = lst3[key3]['classspeedup_all'].keys()
              for opt in sopts:
                test_speedup[opt] = {}
                for (speedup,name) in lst3[key3]['classspeedup_all'][opt]:
                  test_speedup[opt][name] = speedup
                  if name not in tblnames:
                    tblnames[name] = True
              # Compare pairs (for the Schulze voting method, a Condorcet voting method)
              d_matrix = [[0 for y in sopts] for x in sopts]
              len_sopts = len(sopts)
              for y in xrange(len_sopts):
                for x in xrange(y+1, len_sopts):
                  for n in tblnames:
                    if test_speedup[sopts[x]][n] > test_speedup[sopts[y]][n]:
                      d_matrix[x][y]+=1
                    elif test_speedup[sopts[x]][n] < test_speedup[sopts[y]][n]:
                      d_matrix[y][x]+=1
                    else:
                      pass
              # Calculate the best option for each test
              best_opt_for_test = dict([(name, ('none', 0)) for name in tblnames])
              for opt in lst3[key3]['classspeedup_all']:
                for (speedup,name) in lst3[key3]['classspeedup_all'][opt]:
                  (best_opt, best_speedup) = best_opt_for_test[name]
                  if speedup > best_speedup:
                    best_opt_for_test[name] = (opt, speedup)
              # Calculate the worst option for each test
              worst_opt_for_test = dict([(name, ('none', 100000)) for name in tblnames])
              for opt in lst3[key3]['classspeedup_all']:
                for (speedup,name) in lst3[key3]['classspeedup_all'][opt]:
                  (worst_opt, worst_speedup) = worst_opt_for_test[name]
                  if speedup < worst_speedup:
                    worst_opt_for_test[name] = (opt, speedup)
              # Calculate the winner vs. loser average of the best speedups that can be obtained per-test
              wl_speedup_for_test = {}
              for name in best_opt_for_test:
                (_, best_speedup) = best_opt_for_test[name]
                (_, worst_speedup) = worst_opt_for_test[name]
                wl_speedup_for_test[name] = best_speedup/worst_speedup
              # Calculate the average of the best speedups that can be obtained per-test
              sabs_speedup = {'mean':0, 'max':0, 'min':100000}
              for name in best_opt_for_test:
                (_, best_speedup) = best_opt_for_test[name]
                sabs_speedup['mean'] += best_speedup
                sabs_speedup['max'] = max(sabs_speedup['max'], best_speedup)
                sabs_speedup['min'] = min(sabs_speedup['min'], best_speedup)
              sabs_speedup['mean'] /= len(best_opt_for_test.keys())
              # Calculate the winner vs. loser average of the best speedups that can be obtained per-test
              wl_sabs_speedup = {'mean':0, 'max':0, 'min':100000}
              for name in best_opt_for_test:
                speedup = wl_speedup_for_test[name]
                wl_sabs_speedup['mean'] += speedup
                wl_sabs_speedup['max'] = max(wl_sabs_speedup['max'], speedup)
                wl_sabs_speedup['min'] = min(wl_sabs_speedup['min'], speedup)
              wl_sabs_speedup['mean'] /= len(best_opt_for_test.keys())
              #
              # When key3 is possible, the number of times that each option wins are lst3[key3]['count']
              key_name = '|'.join([inf_opt.optname[x] for x in key3.split('|')])
              print >>self.outfile, "<h4>%d cases for each option %s</h4>" %(lst3[key3]['count'], key_name)
              # (Print pairwise comparison table)
              print >>self.outfile, "<p>Pairwise comparison table (d[row][col] = number of tests where test[row] is faster than test[col])</p>"
              print >>self.outfile, "<table>"
              str="<td class='empty_top_left'>&nbsp</td>"
              for x in xrange(len_sopts):
                str += "<th>(%d)</th>" % (x)
              print >>self.outfile, "<tr>%s</tr>" % (str)
              for y in xrange(len_sopts):
                str="<td>(%d) %s</td>" %(y, inf_opt.optname[sopts[y]])
                for x in xrange(len_sopts):
                  str += "<td>%d</td>" % (d_matrix[y][x])
                print >>self.outfile, "<tr>%s</tr>" % (str)
              print >>self.outfile, "</table>"
              # Schulze method, based on http://en.wikipedia.org/wiki/Schulze_method
              winners_list = self.schulze_winners(sopts, d_matrix)
              winners_list_names = [[inf_opt.optname[y] for y in x] for x in winners_list]
              print >>self.outfile, "<p>Preference list (modified Schulze method): %s</p>" % (winners_list_names)
              if len(pinf_opt.list) == 0:
                # We are not analyzing any option projection
                # In case of a draw, add the candidate with the best mean speedup
                abs_winner = self.absolute_schulze_winner(winners_list, lst3[key3]['classspeedup_mean'])
                abs_loser = self.absolute_schulze_loser(winners_list, lst3[key3]['classspeedup_mean'])
                for who in [abs_winner, abs_loser]:
                  str = "<td>%s</td>" %(self.all_opt.optname[who])
                  self.summary_str[time_mode][cinf].append(str)
                  for attr in ['classspeedup_mean', 'classspeedup_max', 'classspeedup_min']:
                    speedup = lst3[key3][attr][who]
                    str = "<td class='%s'>%.2f</td>" % (speedup_style_class(speedup), speedup)
                    self.summary_str[time_mode][cinf].append(str)
                # W/L speedup (speedup of the best case w.r.t. the worst case)
                # NOTE: do not use averaged speedups, but individual test cases! (and then calculate the mean)
                winner_vs_loser = {'mean': 0, 'max': 0, 'min': 100000}
                for name in tblnames:
                  speedup = test_speedup[abs_winner][name] / test_speedup[abs_loser][name]
                  winner_vs_loser['mean'] += speedup
                  winner_vs_loser['max'] = max(winner_vs_loser['max'], speedup)
                  winner_vs_loser['min'] = min(winner_vs_loser['min'], speedup)
                winner_vs_loser['mean'] /= len(tblnames.keys())
                for what in ['mean', 'max', 'min']:
                  str = "<td class='%s'>%.2f</td>" % (speedup_style_class(winner_vs_loser[what]), winner_vs_loser[what])
                  self.summary_str[time_mode][cinf].append(str)
                # Sabs mean speedup
                for what in ['mean', 'max', 'min']:
                  str = "<td class='%s'>%.2f</td>" % (speedup_style_class(sabs_speedup[what]), sabs_speedup[what])
                  self.sabs_summary_str[time_mode][cinf].append(str)
                # W/L Sabs mean speedup
                for what in ['mean', 'max', 'min']:
                  str = "<td class='%s'>%.2f</td>" % (speedup_style_class(wl_sabs_speedup[what]), wl_sabs_speedup[what])
                  self.sabs_summary_str[time_mode][cinf].append(str)

              # Write score using the order in winner_list
              score_i = len_sopts
              for a in winners_list:
                for b in a:
                  lst3[key3]['score'][b] = score_i
                  score_i -= 1

              # Print values for each key
              print >>self.outfile, "<p>Options sorted by the preference list:</p>"
              results=[]
              print >>self.outfile, "<table>"
              str="<th>%s</th>" %(inf_opt.name)
              #for x in ['ranklist', 'mean speedup', 'max speedup', 'min speedup']:
              for x in ['mean speedup', 'max speedup', 'min speedup']:
                str+="<th>%s</th>" %(x)
              print >>self.outfile, "<tr>%s</tr>" %(str)
              for x in lst3[key3]['score']:
                str="<td>%s</td>" %(inf_opt.optname[x])
                str+="<td>%.2f</td>" %(lst3[key3]['classspeedup_mean'][x])
                str+="<td>%.2f</td>" %(lst3[key3]['classspeedup_max'][x])
                str+="<td>%.2f</td>" %(lst3[key3]['classspeedup_min'][x])
                results.append((lst3[key3]['score'][x], str))
              results.sort()
              results.reverse()
              for (_,x) in results:
                print >>self.outfile, "<tr>%s</tr>" %(x)
              print >>self.outfile, "</table>"
              # Print detailed per-test, ordering speedups
              print >>self.outfile, "<p>Detailed speedup table, sorted by the preference list (best option for each test is marked in <span class='marked'>red</span>):</p>"
              results=[]
              for opt in lst3[key3]['classspeedup_all']:
                str="<td>%s</td>" %(inf_opt.optname[opt])
                testlen = len(lst3[key3]['classspeedup_all'][opt])
                for (speedup,name) in lst3[key3]['classspeedup_all'][opt]:
                  (best_opt, _) = best_opt_for_test[name]
                  str += speedup_and_test_html_td(speedup, name, opt == best_opt)
                results.append((lst3[key3]['score'][opt], str))
              results.sort()
              results.reverse()
              print >>self.outfile, "<table>"
              print >>self.outfile, "<tr><th>%s</th><th colspan='%d'>speedup per test</th></tr>" % (inf_opt.name,testlen)
              for (_,x) in results:
                print >>self.outfile, "<tr>%s</tr>" %(x)
              print >>self.outfile, "</table>"
              # Print detailed per-test speedups
              print >>self.outfile, "<p>Detailed per-test table, sorted by the preference list (best option for each test is marked in <span class='marked'>red</span>):</p>"
              results=[]
              for opt in lst3[key3]['classspeedup_all']:
                str="<td>%s</td>" %(inf_opt.optname[opt])
                tblitems={}
                tblnames={}
                for (speedup,name) in lst3[key3]['classspeedup_all'][opt]:
                  if name not in tblnames:
                    tblnames[name] = True
                  tblitems[name] = speedup
                for name in tblnames:
                  speedup = tblitems[name]
                  (best_opt, _) = best_opt_for_test[name]
                  str += speedup_html_td(speedup, opt == best_opt)
                results.append((lst3[key3]['score'][opt], str))
              results.sort()
              results.reverse()
              print >>self.outfile, "<table>"
              str="<th>%s</th>" %(inf_opt.name)
              for name in tblnames:
                speedup = tblitems[name]
                str += "<th>%s</th>" % (name)
              print >>self.outfile, "<tr>%s</tr>" % (str)
              for (_,str) in results:
                print >>self.outfile, "<tr>%s</tr>" %(str)
              print >>self.outfile, "</table>"
              # Print the W/L speedups that can be obtained per-test
              print >>self.outfile, "<p>Winner vs. loser speedup per test, using the best option (and individual abstract machine):</p>"
              print >>self.outfile, "<table>"
              str=""
              for name in wl_speedup_for_test:
                str += "<th>%s</th>" % (name)
              print >>self.outfile, "<tr>%s</tr>" %(str)
              str=""
              for name in wl_speedup_for_test:
                speedup = wl_speedup_for_test[name]
                str += speedup_html_td(speedup, False)
              print >>self.outfile, "<tr>%s</tr>" %(str)
              print >>self.outfile, "</table>"
            print >>self.outfile, "</div>"
          print >>self.outfile, "</div>"
        print >>self.outfile, "</div>"

  def infer_good_opts2__summary(self):
    # Table of speed-ups for all tests at the same time
    print >>self.outfile, "<p>Winner and speedup table for each address space for machine <b>%s</b>:</p>" %(self.machine_name)
    print >>self.outfile, "<table>"
    who_names = ['Winner', 'Loser']
    # time mode labels
    str="<td class='empty_top_left_bottom'>&nbsp</td>"
    for time_mode in self.time_modes:
      str += "<th colspan='11'>%s</th>" %(self.time_mode_name[time_mode])
    print >>self.outfile, "<tr>%s</tr>" % (str)
    # winner/loser labels
    str="<td class='empty_top_left'>&nbsp</td>"
    for time_mode in self.time_modes:
      for who_name in who_names:
        str += "<th colspan='4'>%s</th>" %(who_name)
      str += "<th colspan='3'>W/L</th>"
    print >>self.outfile, "<tr>%s</tr>" % (str)
    # opts mean max and min labels
    str="<th>Address space</th>"
    for time_mode in self.time_modes:
      for who_name in who_names:
        str += "<th>opts</th>"
        str += "<th>mean</th>"
        str += "<th>max</th>"
        str += "<th>min</th>"
      # W/L
      str += "<th>mean</th>"
      str += "<th>max</th>"
      str += "<th>min</th>"
    print >>self.outfile, "<tr>%s</tr>" % (str)
    for cinf in self.sorted_cinfs:
      str = "<td>%s</td>" %(self.addr_opt.optname[cinf])
      for time_mode in self.time_modes:
        for x in self.summary_str[time_mode][cinf]:
          str += x
      print >>self.outfile, "<tr>%s</tr>" % (str)
    print >>self.outfile, "</table>"

  def infer_good_opts2__sabs_summary(self):
    # Table of speed-ups, specializing for each test
    print >>self.outfile, "<p>Mean speedup table, for each address space, specializing for each tests, for machine <b>%s</b>:</p>" %(self.machine_name)
    print >>self.outfile, "<table>"
    # time mode labels
    str="<td class='empty_top_left_bottom'>&nbsp</td>"
    for time_mode in self.time_modes:
      str += "<th colspan='6'>%s</th>" %(self.time_mode_name[time_mode])
    print >>self.outfile, "<tr>%s</tr>" % (str)
    # winner/loser labels
    str="<td class='empty_top_left'>&nbsp</td>"
    for time_mode in self.time_modes:
      str += "<th colspan='3'>Sabs</th>" # speedup that can be obtained using the best option for each test (a specialized abstract machine per test)
      str += "<th colspan='3'>W/L Sabs</th>" # sabs but using the winner vs loser speedup per test
    print >>self.outfile, "<tr>%s</tr>" % (str)
    # opts mean max and min labels
    str="<th>Address space</th>"
    for time_mode in self.time_modes:
      # Sabs
      str += "<th>mean</th>"
      str += "<th>max</th>"
      str += "<th>min</th>"
      # W/L Sabs
      str += "<th>mean</th>"
      str += "<th>max</th>"
      str += "<th>min</th>"
    print >>self.outfile, "<tr>%s</tr>" % (str)
    for cinf in self.sorted_cinfs:
      str = "<td>%s</td>" %(self.addr_opt.optname[cinf])
      for time_mode in self.time_modes:
        for x in self.sabs_summary_str[time_mode][cinf]:
          str += x
      print >>self.outfile, "<tr>%s</tr>" % (str)
    print >>self.outfile, "</table>"

  def preprocess(self):
    # Show speedup per benchmark and options
    for opts in self.opts_list:
      print "opt([%s])." % opts.replace('|',',')
      print "try(1)."
      for name in self.names_list:
        print "test(%s)." % name
        key = name+'-'+opts
        print "resource(time_without_gc,%f)." %(self.time_set[key])
        print "resource(time_with_gc,%f)." %(self.time_set[key]+self.gc_time_set[key])
#        print "resource(gc_time,%f)." %(self.gc_time_set[key])
        print "bytecode_size(unknown)."

def help():
  print "Usage: summary ACTION"
  print "Where ACTION is:"
  print "  [full|compact] RESULTSFILE      Show full or compact summary"
  print "  infer RESULTSFILE               Infer good options"
  print "  preprocess RESULTSFILE"

def emit_css_style(outfile):
  print >>outfile, """
<style type="text/css">
  body {
    padding-left: 4em;
    padding-top: 1em;
    font-family: Helvetica, Geneva, Arial, sans-serif;
    font-size: 10pt;
    color: black;
    background-color: white; 
  }
  h1 {
    font-family: Helvetica, Geneva, Arial, sans-serif;
    color: #202020;
  }
  div.info {
    margin: 1ex;
    font-family: Helvetica, Geneva, Arial, sans-serif;
    color: #505050;
    font-size: 8pt;
  }
  span.marked {
    color: red;
  }
  table {
    border: 1px solid #000000;
    border-collapse: collapse;
    font-size: 8pt;
  }
  th {
    font-style: bold;
    margin: 0px;
    padding: 2px;
    border: 1px solid #000000;
  }
  td {
    margin: 0px;
    padding: 2px;
    border: 1px solid #000000;
  }
  td.empty_top_left {
    border-top: 1px solid white;
    border-left: 1px solid white;
    border-bottom: 1px solid #000000;
    border-right: 1px solid #000000;
  }
  td.empty_top_left_bottom {
    border-top: 1px solid white;
    border-left: 1px solid white;
    border-bottom: 1px solid white;
    border-right: 1px solid #000000;
  }
  td.speedup_good3 { background-color: #ff8040; }
  td.speedup_good2 { background-color: #ffc040; }
  td.speedup_good { background-color: #ffff80; }
  td.speedup_bad { background-color: #ffffff; }
  td.speedup_bad2 { background-color: #c0ffff; }
  td.speedup_bad3 { background-color: #80ff40; }
  td.speedup_bad4 { background-color: #4080ff; }
  div.inner {
    padding-left: 2em;
  }
  div.normal {
    padding-left: 0em;
  }
</style>
"""

def begin_html(outfile, title):
  print >>outfile, "<html>"
  print >>outfile, "<head>"
  print >>outfile, "<title>%s</title>" %(title)
  emit_css_style(outfile)
  print >>outfile, "</head>"
  print >>outfile, "<body>"

def end_html(outfile):
  print >>outfile, "</body>"
  print >>outfile, "</html>"

# TODO: is the condorcet method correct?
# TODO: differences between my modified Schulze method vs. the
#   Kemeny-Young method?
# TODO: mark tests that are not very optimized using the inferred
# good option set
# TODO: DO NOT SHOW LIMITS FOR GCed memory! (that makes no sense, the
# GCed memory column purpose is just showing how much memory is
# collected for each address space option set)

# NOTE: We assume that there are a fixed number of tags in all the
# tag scheme variations. Results may be different for a different number
# of tags, etc. (that would be a different study)


def emit_intro(outfile):
  print >>outfile, """
<h1>Analysis of benchmark results</h1>
<p>Each address space is identified by the set of options that defines
size of the tagged word and the available bits to encode pointers.
For each address space, the combination of several sets of options is
analyzed.</p>

<p>Winners are obtained using a modified Condorcet method: it obtains
the winner using the Schulze method, then removes the winner from the
option list and calculates the next winner. That process is repeated
until the candidate list is empty. That results in a preference
list. The Schulze method may find a draw between two or more
candidates, in this modified algorithm, the absolute winner is the one
with the best average speedup</p>

<p>Times are normalized for each machine w.r.t. a unique case. The
speedup shown here is relative to that unique case.</p>

<p></p>

"""

def speedup_and_test_html_td(speedup, name, is_best):
  if is_best:
    return "<td class='%s'><span class='marked'>%.2f</span><br/>%s</td>" % (speedup_style_class(speedup),speedup,name)
  else:
    return "<td class='%s'>%.2f<br/>%s</td>" % (speedup_style_class(speedup),speedup,name)

def speedup_html_td(speedup, is_best):
  if is_best:
    return "<td class='%s'><span class='marked'>%.2f</span></td>" % (speedup_style_class(speedup),speedup)
  else:
    return "<td class='%s'>%.2f</td>" % (speedup_style_class(speedup),speedup)

def speedup_style_class(speedup):
  if speedup >= 1.2:
    style = 'speedup_good3'
  elif speedup >= 1.1:
    style = 'speedup_good2'
  elif speedup >= 1.0:
    style = 'speedup_good'
  elif speedup >= 0.9:
    style = 'speedup_bad'
  elif speedup >= 0.8:
    style = 'speedup_bad2'
  elif speedup >= 0.7:
    style = 'speedup_bad3'
  else:
    style = 'speedup_bad4'
  return style

if __name__ == "__main__":  
  if sys.argv[1] == 'preprocess' and len(sys.argv) == 3:
    filename = sys.argv[2]
    s = Summary()
    s.read_report('(no machine)', filename)
    s.preprocess()
  elif sys.argv[1] == 'infer' and len(sys.argv) >= 5:
    output_dir = sys.argv[2]
    input_dir = sys.argv[3]
    machine_names = sys.argv[4:]

    s = {}
    for machine_name in machine_names:
      print "Reading report for machine %s" % (machine_name)
      filename = input_dir + "/" + machine_name + ".txt.gz"
      s[machine_name] = Summary()
      s[machine_name].read_report(machine_name, filename)
      s[machine_name].infer_good_opts__classify_opts()

    print "Writing HTML output..."
    output_filename = output_dir + "/" + "infer.html"
    outfile = open(output_filename, "w")
    begin_html(outfile, "Abstract Machine Generation Versions - Analysis of benchmark results")
    print >>outfile, "<div class='info'>This report was automatically generated on %s -- Jose F. Morales (jfran@clip.dia.fi.upm.es)</div>" % (datetime.datetime.now())
    emit_intro(outfile)
    print >>outfile, "<p>The default case is %s</p>" %(defopts)
    for machine_name in machine_names:
      detailed_output_filename = output_dir + "/" + "infer_%s.html" %(machine_name)
      detailed_outfile = open(detailed_output_filename, "w")
      begin_html(detailed_outfile, "Abstract Machine Generation Versions - Analysis of benchmark results")
      s[machine_name].set_outfile(detailed_outfile)
      s[machine_name].infer_good_opts2__detailed()
      end_html(detailed_outfile)
      detailed_outfile.close()
    print >>outfile, "<h3>Memory usage</h3>"
    print >>outfile, """
<p>NOTE: <b>limits</b> measure the maximum addressable space in MB,
<b>eff limits</b> is an estimation of the equivalent addressable space
w.r.t. the default case (e.g. a 4GB address space can store the same
number of 8 byte taggeds than a 2GB address space with 4 byte
taggeds)</p>
"""
    for machine_name in machine_names:
      s[machine_name].set_outfile(outfile)
      s[machine_name].infer_good_opts2__memory()
    print >>outfile, "<h3>Summary of speedup results</h3>"
    for machine_name in machine_names:
      s[machine_name].set_outfile(outfile)
      s[machine_name].infer_good_opts2__summary()
    print >>outfile, "<h3>Summary of speedup results using the best options for each test (an absmach for each test)</h3>"
    for machine_name in machine_names:
      s[machine_name].set_outfile(outfile)
      s[machine_name].infer_good_opts2__sabs_summary()
    print >>outfile, "<h3>Detailed results</h3>"
    for machine_name in machine_names:
      s[machine_name].set_outfile(outfile)
      s[machine_name].infer_good_opts2__detailed_links()
    end_html(outfile)
    outfile.close()
    print "Done"
  elif (sys.argv[1] == 'full' or sys.argv[1] == 'compact') and len(sys.argv) == 3:
    detailed = sys.argv[1]
    filename = sys.argv[2]
    s = Summary()
    s.read_report('(no machine)', filename)
    s.report_memory_usage(detailed)
    s.report_time(detailed)
  else:
    help()
    sys.exit(1)
