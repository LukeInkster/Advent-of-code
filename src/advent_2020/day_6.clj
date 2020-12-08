(ns advent-2020.day-6
  (:require [clojure.string :as string]))


(defn part-1
  [input]
  (reduce + (map #(count (distinct (string/replace % "\n" ""))) (string/split input #"\n\n"))))


(defn part-2
  [input]
  (reduce
    +
    (map (fn [group-str]
           (let [forms (map (partial into #{}) (string/split-lines group-str))]
             (count (filter (fn [question] (every? #(contains? % question) (rest forms))) (first forms)))))
         (string/split input #"\n\n"))))


(def small-input
  "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb")


(def large-input
  "bapocnysdr\nlpandcmb\nbplndca\n\nrgi\nci\ni\niv\n\nxdgwtsc\ngtcxswd\nsdcxtwg\n\ng\nj\nk\n\ndrcmwzh\naostudi\n\nqziunh\nhgkaslmyz\n\nfovlpdhurzqsway\nzvciokytxbaf\n\ny\ny\nyp\ny\ny\n\ngqouslwfihtxvke\nbapdmyncjzr\n\nywirnpmozqle\njhapfdzuvgc\n\nce\nevn\nyzsem\n\nphdcuxgtosjier\njfixnurtyopdh\n\nmxjs\nmxkjis\njxsm\nxvmbsj\nsxjcmh\n\nsgrbtmqxpwkacnzd\nbrncqxtskawgpzdm\nmxqjtcpzbswgrnka\n\nfxijprlkbhte\nherpxfbikmtlj\n\nltuyidqkcrevanw\nwhbqavirludxtye\nvfirqwmsaopyzld\n\nq\nq\nq\n\nfaxhswigtprou\nxhlusnijytkrwza\nxaqsburiftwh\nrtahxwcsiu\n\ncuplrimdtxke\nletpjuimzrvkcyx\ncpleiuxdktmar\nwrfgkubeoqxsihpntmc\n\nzpbjimulvrdwxsog\nhfmspnuokbxe\n\nctm\nctm\nmct\n\ntvkocqebls\nqcnbktsveo\nfnvkbscqyeot\nmbcsoevkthqj\n\ntyjgd\ntydjg\ntgdyj\ndtgyj\n\nxgiybeuhtkfdonq\ndiuqvfycrokhgxbt\nigfdbhpqklxuyntjo\nzxqkymfohstuiwdgb\nkgoutiyhfxqcbd\n\nwzfiaexksp\nfizxyskw\n\nazsqntcghjmio\nmgksdwpcitvxqjr\n\nctwzu\njwctuzes\n\nbpymqfisrvtuheljwadx\niejsubvxhdmwpqgfrltya\njbxmfshuvyegwqlpridta\nvpxbwlqumetdrijhfnays\n\nw\ni\ni\ni\ni\n\nfctbvjmnroypuxhlakzw\ncbpnmyzrtjuavfohkxlw\nznoptxwakjfucmhrlvy\nnkmlrhygxcazowtpquvjf\n\nibanduczjfmpt\ngqimbtadcuoszjfprl\nmtaizxcdjpbfu\nmjfaiubcdzpt\n\nkdxav\nmaxvudr\nadxv\nqxavdi\n\nuajvepgoxhwsqkym\nhwesxqvpuzgb\n\nlp\npl\npl\nlp\n\ntawk\nsuwtc\nstw\nixltp\nwto\n\nphrawmes\nbscrhki\n\nxhwurk\nhwea\nbhwe\n\ntuhvebm\nbemtuhv\n\nprziuqhcye\nqipyrhezcu\nueqzyprhci\nyurseihcqpz\n\nmnfotvpzdxbaglye\nnbjlomxzacfqgey\nglxhuwoskyrabi\n\ntohbrluismcagnypqdv\nduqbhylatcvosnprimg\nqtpbduycrlosngivhma\nnvgucpayidlhsomrbqt\n\ngrslqxjdytzficnuo\nklarqjpxfscgyouzi\n\nqozstka\npjnruvic\nfchup\n\nyjautz\najue\nvlufjx\nujy\n\ninythxv\nbhxvytin\nhtlnxviy\n\nmhcqvt\nsqtvchm\nhtqmvc\n\nvswymdnahrq\nhrnqvysawmd\nndsmqvwyrah\nmraywhsvndq\n\njtxayp\natjxp\njpraxgt\njatpx\nadbwpjltx\n\nvbni\nivgfn\navyqtmsikn\nnhwivrf\n\nfkygvtsbpazdjwxrmqoi\nbtyfwsvpdiarjqoxmg\nwvqyiaprsmftgxdojb\n\na\na\nan\n\npynvsfcbherzqiwumjaod\ndsivqjmurcfnzebyhopaw\nmqsvnueoihczyrfdwpbja\ngjqsaiwdczpvublmnokfryeh\n\ndrawsthnfvzylebo\njmkzqhbgicxsu\n\npg\nhdob\nnq\n\ngdbxpwmtvhoaely\nomvdgatwbxlpyeh\ngtbyxrvwmhladpeo\n\nugowlqebtcpdxz\nwzobutlqgdxep\ntulbvxwfadjpogqze\nqubdxgztpknolwe\nepztwlbxqgodu\n\nrsvebtycq\nbecltsrnvw\n\ntgsljhz\nzhltjs\nhsjltxz\n\ndaguc\nfbpudxyg\nglidjuo\ngoadus\nudjgaz\n\nqkpwetasjfbdvh\nglixc\n\npufekjh\nxegpq\npoewviyn\nenlbvp\ntpesm\n\nzqfiwx\nzqfriw\nwfzxil\nzwxu\ndznwktpb\n\nkcaomdbszyi\nildpzaqsbx\nxsdqirazb\n\nmqebolfk\npeqmgf\ndeqfmw\n\ngrcqpi\ndvpmqte\nohlwfujxkanbsyz\n\ntxnhgzuwaicrfl\nqtnwizslxuahocgr\nmcxhglnibardwtkuz\n\ncvjbyrewa\nwczb\nzbwc\nwcb\n\npsvotmiaqyuhdnrekw\naovnrqwsiyjmdutpehx\ngaephqunbrwdsmyiltov\nrmftshdpcznwiyuoaveq\npyatoimwhxeusnvrqd\n\nwsjvydieaurhqxofzlgpmbk\nkemhzfusnpajxirywlo\n\nypiz\nipz\njwiz\niz\n\npxoedfk\npxogkd\ntkjsdocpex\n\nlsoz\nzxc\nz\nwkz\nzpw\n\nfrmcndwyeix\nqritwfuymc\nmzrwyfixc\nmfczprywi\n\nygvtslukaeoinhwd\nvnepzqmcwyjlufrdobk\n\nbi\nitb\nbi\n\nwy\nqomwberkixc\ngw\nhswg\nlpw\n\nkrifxwcusnjmle\ncnijrwxkmspf\ncnrfiegmqswkxjh\nskiyrmwxcdzjnf\n\nifwqlzhrudsmn\nfzblomnwspjvk\n\nvnjsxdiobeuhcgfw\ndhwienfjsobgcvux\nwsinxchtegujbdvof\ncioehdxvnjgwbusf\ngdcsfeobjrunhxwviy\n\nlpzntyhsxruiqjckwv\nixyprghzqfvjsuotlw\nayltehsxubqpmwrdjivz\n\nr\nlmb\n\nn\nn\nn\nk\n\nlfsxwgkeioztnuydmqbc\nwkosmqcxtdnluebigyfz\nolekfdcyxgqtsbumzwin\nkwqzfedxblcitgmnuoys\nibdetugcmfzoknwlxsyq\n\nc\nc\nc\nc\n\nnmrohgpcwsjadkyqzl\nmgjhdwcpoknsqyazr\nchnajzmwopkgdyrqs\nbdcnhgamkjqysropfzw\n\nyqtid\nujgkz\nrmas\njk\nog\n\nf\ncrj\ncyo\nneqhmb\nf\n\nuwxc\nwpcu\n\nbkhpcqyeljvftxnz\nkpvxqcjin\n\neqjzkctynvo\nqyovecnjgkt\nxjckqenybvhpto\njnkmezvyctqo\n\nlon\nw\nvwh\nh\n\nzrosv\noblzfats\ncuozbmak\ndpyegwhnjz\noztiqx\n\nbvlwgjrixaskqpuzyonth\nhatusdrovjgwcyipeqkmzln\n\nvhneajldr\napnvjderl\nlynrjcae\n\nqodczyfijam\nfkdxpgrcnmio\n\noai\niao\noia\noai\nioa\n\nnhgewpbozmjascrk\nvsocxumhryfnbqwkaj\n\njxrls\nsycjpwr\nbdcrqenuhtsz\nsravkif\n\nbymwecxqrloj\nlhofkz\nvzldo\nklof\noulk\n\nlfxobdwcji\nodbpnilxjcw\n\npbcr\nrcpb\n\nmsp\nesm\nsm\npsm\ndcsm\n\nclkiabts\nhskec\nsckgf\nkfncs\nsekc\n\ndyckmholpqbtaunf\nlacdqknfhuymtopb\nkntfqadpjlbcyhumo\nqoflmbpdhauykctn\ndphmolaynqcfbtku\n\nwhkzitxfqem\nftixzqmrwek\nmitabqyxkzjwfpne\nqzwekxtmfi\niztmkfwxeqd\n\nwxktrn\ntxr\n\ntapvryqbecdklwsjzxug\nmwjgatucpvydsnqbklxr\n\ntw\nwt\nwt\n\nirsehx\nehsxri\nrhxise\n\nekwyuzsvpfjg\nlcspakbhnmiod\n\nqifdbzapc\nrwpvkbd\n\nf\nf\nf\nf\nf\n\nyskpcdmber\nbnpdelmcryksu\neqcbzdpkgyrmsixj\n\nibt\niez\ncz\nb\nxvls\n\nd\nd\nd\nd\ndq\n\nubfpqyrkhceo\nyfhqckubre\n\nzyhnmseuivlbxgfk\nnviufhobsyelxzkm\n\nagbdzfsqetclpw\nwctbfaqeszpgdl\ncsbtraldqepvgzfiw\n\ndmatzvowqcklbgirf\nldiczavboqgwmfktr\ngkaditvmclqrwfboz\ndvrfntlcijagmzobuqkw\niwzrtblvdoamqkfcg\n\nxhjydaekowcgsmruftbpqvln\nwzryxjmfdloqvskubanh\n\nskglcvahyx\nxyp\nyx\n\ntvsguaxobzkd\ntxoguzsmkbv\nsoegkitjbxyuzv\nkvobgsztux\ngtxvcspoukqfhznb\n\nosca\nosai\n\nsjv\nsjv\nvusofja\nvjs\nsjv\n\nipwxnmkbzhscarlejtdoufqyg\nujlxyqzkgtpfrdnihwaomscbe\ndyslimrkjqxnftcpbewzuohga\ngyphacbxnqdriweumloszjktf\nczbroamputiwjdnqhleyksgxf\n\ndygktjflepbcqm\nzfwsneviuo\n\nusz\nusez\nusz\nzscu\n\njkbies\nejikm\nejphkoi\nkmjseiz\njike\n\nqwjvbzmypcif\nhveligmbpatow\n\naumeftxligdy\ntylfgicdx\nwlditgfyx\ntcxdgrilfsy\n\nwi\niw\niw\niw\nwi\n\nzsd\ncbmoz\nouwbaq\nghexrpyf\nasmz\n\nax\nw\nwrd\nvqclmnjfob\n\nsy\naumz\nazs\nhvoecbdfgq\n\np\np\np\npo\np\n\nsydzgpwourlbqfcj\nmnzcjkeubaprofg\n\nkiatxsph\niphktsa\nwaplhikjt\nthapik\n\ncpv\npcve\nvpc\nlmtcv\n\neyghlrunfctbqzos\nnsryfzuqtmecol\n\nzayxpivkctudsmgrhqfl\nynfsuctxhakdpjglmovq\nfdvgeskyaublpmhctrwqx\n\negymspiuwoarnlj\npoidtzjyglwume\n\ntmlv\nmotv\nmvthn\n\nvh\nvh\nvh\nvh\nvh\n\nvthobzjux\nojutxvhbz\nvxhojbzut\nzhtvbxouj\n\nk\nk\ne\nk\n\nbkwvlpt\nxormzn\n\naforqletdymv\neyorqmflvdta\nytvaqoejmdlrf\n\nrqakhn\nkhrqusmaj\nrkhqa\n\nbmyztkxldgiosnw\ntsoyzwixdmlbnkg\nbtimzdlywknsgxo\n\nmqifeszchaoxjygvdrklubwn\nzjdhxnboagfrc\ncgarozjxdnbfph\nxbnrhcjzfgaod\nfzdhrxtancjobg\n\ngzdvh\ngztvmch\nzgdhv\nghzv\n\nyjldgioabsr\nmusahntecwl\n\nfujnlstvgcbwio\nclgkwmijsupqzbo\n\ncqi\niqga\nqza\nelgyxq\ndujqtkprvf\n\ndzrawsxvhgtflnjeikoubqypm\npryqevnbkiusghjlofadztxw\neawipgrutyohqvsbcknjzfxld\nbavdzsphqugykftnjeorilxw\n\nq\nvxoln\nfeq\n\ny\ns\ns\nl\n\ngfdp\npgd\naep\npdfsb\n\nwatcn\ntawn\n\nl\nnly\nloy\ngwsvl\nl\n\nehfqltbkvizcmpaxn\nvzqkecthbmnfxliap\n\nl\nlrx\nl\nel\n\nmznoqpxfvedutclhar\ngsibywkj\n\ngft\ndqs\nrm\nrbe\ndnsrh\n\ndgwncrbfo\nuboewdjnagxcr\nogrcdbnyw\nbwgrdnoc\norwbcgnd\n\nxlnyqidbvfuopwsmetkaz\naxsnipqkotbydfvuzle\nzsyaohudvqweikfpljxnbt\naezfhxqipvkdtbnoyuwsl\nqsneybxfzidcagokulptv\n\nphnrogdiuwlqtmfvjcb\nsdmufyklricxpg\nifdxpmkruealgzc\n\nxdiushclzbkvnfw\nbjmzunysickrfvxd\nxciutvyksbldfzn\nxfqpzabiuecnogsvkd\n\nolv\novl\nlov\novl\n\njirywmg\njnxmg\njfbgm\nmjhg\n\nsjfvbhimpyauqctgx\nxasifmnuqtyvgwojclp\njxyqabfuismptgcv\n\nspgmtcvwlxyfaq\ngpnqlctymxw\n\nxthqemkfpyls\npqhtsymrgzoef\nyskhmtfpqe\n\nxcsqaokhpt\nhcopsqtxak\npqoaxstchk\n\noqexlariumftkcg\nxceqoumklaiwnhtf\nxekitmlufoqcag\n\nzkjmutfxryowbnv\neuomntlvzxiqydrgwspa\nwhcyrmvozxktun\n\nhxqncgbjrsl\nbnlxcjrqhs\ncjqngsblxhr\nihpsnrblcxqj\n\nrjtlufdmxiqkn\nafmrkuindxljt\nuktrmjilxnfd\n\nypxqris\nhpgk\nbudwp\n\noqulm\nu\nu\niu\n\nkahocwxnueyri\nrzesvkxyltmbpqg\n\npkxjboidfvymngzrts\nrsizgmydxkpfvbtnjo\nbopgismxvrzntydfjk\novgfdztnspyxibkrmj\noecxdvmgrfkzbnypijts\n\nruvmcpojzsqtayw\nrqotdwzmpjcuys\nptnscirkowujyzqfm\nocszmjwrtyqpu\n\nnzxqdbwvpkioaj\nbqijkzpxwonadv\nkivqoujwxdzpntab\ninxabjvzpqdkow\n\nochlv\ncovl\ncovyl\n\nqsoypnvewrhjkdgtb\njpgwdknqeytbhsovr\nbdpwyrjuhoqgevtsnk\nrkoeypwbvgjnhsqdt\nqgtnyodrepsvkbwjh\n\nu\ni\no\no\nrx\n\necikgqxbyjhupozvdnrstawfm\ncnuiyopkzwmsbtadgeqrfxvj\nomkbnwrszvaetclqdpxfiygju\nwimrdtexbnjspgokuvzaqcyf\nveacgjpzmnioqksfwybxurdt\n\negfkhxi\ngkshf\n\nshiayxrwutmzf\nisyatmhuzf\n\ntfgmicrqnpxz\njeurkptlyh\nkvtphwr\n\ntyqrx\nhotsrdyqz\nyptqr\ntyqr\nryxtq\n\nlxtdzqwcakvm\nxdkqrlzv\n\nipujabzkmldhfrwg\nbjmpkavhlgqrwfdiz\nofhizbrtdcagljwmp\nbfgvdaesjpwrlzumhi\n\ns\ns\ns\ns\nhs\n\nnobqduvlcjtw\ndyrbtpzwng\n\niohlwxgfs\nzokfn\n\nuawlqejvnfkzhor\nrqhkanfvexlwzoju\navqekulojnfrzwh\nkljwrhonqfzauev\n\nln\nnl\nnl\nln\nln\n\nqyxi\nfmxbo\nqxo\npkvnutje\n\ntjsz\nzjr\ngydfxcjliq\n\nufa\ndohwy\nf\nkau\n\nezjqncv\njqvzenc\n\npnhlasjrztw\nrlswpatnjz\nhpntrjslazw\nzjnrwtlapcs\n\ni\ni\ni\ni\ni\n\naovitgfucj\nbowihz\niol\niweoz\niyo\n\nbdgmouhjvyxr\ninpshdz\nnhdfia\nhtdl\n\npnzawmosg\nzganost\n\nemrakn\nglveamkr\nrkgames\nakfqmper\nimudroeak\n\ntibgokrjpxmy\niydtpmjkx\ndkcjpxseiymt\n\nimfnqegj\nmcp\nnwgkhmbrpjac\nszovdtxl\n\nqnosfwmzpiubkxdhctg\nmsutibqnakhgczfopxw\nxtnowmgicfqsuzbhkp\npwbkcqiutxfshzngmo\n\njgaqubtpokrn\njpqgotluzna\n\nsiblotrefupcmyajdwxhvn\ntdxpnfrbuivjchlsawoeym\n\ngvbfkphuqwsrdoejlt\nixpkoqzldfncbahwt\n\nrdlfmyaejnswgopv\nysjfvmazeilonurbpwx\njayvlrwcmpefnos\nyowvefjknltqamscprh\n\nr\nd\nd\nd\nd\n\niry\ngi\nlri\n\nkzopchbqgf\nphcifmsbgz\ncanbyhlvdfpg\n\nljmtd\nstlxdvmkjqy\notcljbfehmd\n\nzgkiqsjrm\nrmqosfenz\nzqdmrsig\n\njaegcxl\ndelcpxa\nlktzbxcea\npdclajxe\n\ngdrfnb\ndngbqriyh\nojnbgrzd\nfdnrbg\n\nixnhlufgkqojyrcdwms\nuvfmspygeizlkhxcawrbdotq\n\ncfzy\nzfcy\nczfy\n\nkfbqdvgacxnluszhejiowmytr\ndurgtzlkjpwnechqibxfamvoys\n\nnjcgoslbyutihpqxwe\nwrbgpdquiykhnofvecztl\n\ncozxyqbvugrmkdwlh\nrxcdwuylztbvgmqhk\nurmhkcdlxvgyqzbw\n\nokzsvutdehimnc\nmednkuhxgqwvizots\n\nsogfalbukwvchqmjdtypzix\npyfazktchbmvsqguojwxild\nimsgakcotjpdhuwbfyzxlqv\n\nyzpafqijdchrvosxkt\ntqvsdrxzojhipfkyc\ntsfqokvphixzdcryj\npjfqzhtxocrdkvyis\n\nysmndq\nqdsnm\nrskilmn\n\nq\nq\nq\nq\n\nfygruq\nfxckylwohur\nfspaznjvbmdt\n\nztqa\na\nnma\na\na\n\nerml\nejrm\nrem\nemr\n\nqkmpeuatxdbrcghnjlo\nxnlmcohutkdbeaqvpfrj\nrboxcdpaujeqhknmtl\n\nbpmiwjrs\n\ntyr\nry\n\njkyolgr\ndv\nanwid\nidzv\n\nz\nxzpnb\nz\nz\nhz\n\nmwxibru\ndxrwzobi\nwixqvrb\nsbrxwi\nbaixrw\n\nyzcdlgujxrpmbs\niadjzpcnyrm\n\nnq\nqn\n\nsbwtamevgyicuhxrlfz\nuzxkpmitwvlfqhcgjn\n\nxvhbkanc\nbozyjftqrl\n\nvpcougdijwlxtsmznarqfy\nqyroagjdmsicwlftvxunpz\nnylpitxuqdwrszamfgjcvo\namogspxyvrdniztqfuwlcj\nfuoiwmpvtlxdszqncrjayg\n\nocmqey\nyqoce\n\nmvulfyk\nfxyuivlmk\nafvulokym\nxufvjmykl\n\nwjz\nowpfqnsrgt\newycah\nwebhjid\nbwi\n\nucxqaszovj\nxacvqjskizuo\n\nauxsjhkiq\nzcymv\n\nqyljukpbitdx\npxqjlduitkyb\nutbpqxjyklid\nyjkdqxptbluir\npuliqbtkdjxy\n\nljurghymqktvpenzsbofwdxc\nofyrjptgnczlwebdxvqmkuhs\nblopcvezhdsjnxgyktqfmrwu\ndwmqkuhtjsofnzgbrvelxcpy\n\nzn\nndz\ntzn\nnkdz\nnzvk\n\nvmyrlzxcwkjp\nmkqlvyrcp\n\nxduvzyfarpkewscjqlmh\nfzcyvxkmhwqreapds\n\nnt\ncs\nc\n\njxraviqybmgutesdwkfohlp\nhnkfweosqvbijpymuxdtalrg\n\nfcjz\nmnwvo\n\nmfqnkiaj\nnakqfimj\narikjubnmqf\nzfqikanjxm\n\npjetdfwsxohlz\nwpkzntbfsdcai\n\nzoifjgyxkluhbrtq\nyjolrgquihbnxktfz\nzoibyqghrjxtkufl\niuklgxbyrtmjfqhovz\n\nr\nr\nr\n\nytqzlvcwseb\nbcvqntlswyez\ncvqbtewyszl\n\nhfyj\njsbrzyhiwknfg\nvfyajhd\ndjfhlym\nfohyjux\n\ndzcjlgutfoipaexkmn\nkoetcadiuznpmljf\nctnilkamodfpzjue\ncofuezanjlpktdim\n\nnvtjmurwhfzex\nthxnrwmvuejaz\njexztumwrhnv\n\nr\nr\nr\na\n\nenoclavqmypruwkb\ntzspyhlawuimvrq\n\nyxcbvjpsgt\ncrxgvitlzjbop\ngukcdbjtwvexp\nbgjcmqspztoxv\n\nw\nw\nw\nw\nw\n\nqmocdhri\nqpwioderm\ndvmocjruiq\n\nnvghcjxd\nnbcvdp\nnvhcd\ndjcvn\n\nyfzukw\nkuywf\nkwsyu\n\nadjzmqebokxhtuspwgiynf\nfmshwkpoguljtyrziabqn\nanyofkgzsbvpuiqthwmj\nqfbhkytzgnwoicjsapmu\n\ntlmn\nnlti\ntyehjn\n\ns\nr\ns\ns\ns\n\nn\nu\nu\n\ntie\njet\net\n\nachjyurqb\njpcbrhutqy\nrbjyuhcqp\nybfucqjhrw\nqujrhbcy\n\no\no\ni\n\nmigypa\npyimag\npmgiya\nagypim\nyaimgp\n\nkdzafylcnst\naznrtshxgylcb\ncstzyaln\nanetplcsyzq\n\nhilubmsyznrtvqxpjo\nyqbirxnpzslojvtumh\ntyxlnibhzmsovrjqup\n\nzloiuyekfpx\naqtxzmig\nwsgxinqbzj\nbavthrdxsiz\n\nacwdny\nghrxysndq\novdncy\n\np\nglxy\n\nksnzbjyxgwtvm\nnbwzvtkjysgxemro\nbvnykztsxjwcmg\nqyzbmxjhtakgnwfdvs\nxzjmbsevgwytnklo\n\nuolyhfmwcpnk\nykuchfolpnwm\nlnckhofpyumw\nylnfuowmrcphk\n\noumqj\nulzybvfsp\num\nu\n\nzh\ngh\nzh\nhns\nhg\n\nxlvaqwncsizrbopukyegdmh\nlxmsgzqyothupikvrbn\n\nlhqdviu\nqafnemdvlzbrpk\nqlvyxdw\nsdluoqygv\n\nv\nv\nv\nv\nv\n\nblqcsyup\nclmpubs\n\nnoumlji\nnouji\n\nwkvj\nd\nghy\nx\nk\n\nohbjykuena\njbyahkpez\njyafhevbk\nyeajhkb\n\nauqjksywpcthxf\nzchkqmanpd\nkvaephqcn\n\nygrvwtleidhapqcoxjzkbm\nuvmtjkcxphybegzqrawiold\ndpckejybntqgvhzaomwixlr\n\nwuvknho\nwunhkv\nawhuixnkv\n\nxtsglwhpkozumein\ntiwzoklpxheumsgn\n\nkdicmfpsejygzlraqh\nrgpqkamdfecyiljzs\nfgjrczaedymqliksp\nadlgcpfieqrmsyjzk\nmfwincakdjelorbygqszp\n\njhtufzbolnrpgiv\nrymwi\n\nbcdfkrgujzqwheptivls\nwlzfyvtimksqcdujngorb\n\neruyigjwb\nwojeybiulrg\nygujbwvzei\n\nfizsg\njnue\n\nyalxikzg\niagxkz\nxizkga\nzikaxg\nikxzag\n\npjzlhqwixksnovbtygr\nljnvgzykoswbtrhm\nrjtnogykswczhlbv\nychwztvrgsonebaljk\n\nwcmyzldhtfrposgnj\nfcrmtyphngjlzs\ngpalbicyvfnmjxekrz\n\nysmpe\nyemps\nstype\n\nmeruv\nmbrse\nryvmsei\nlaremdjq\nbremck\n\njaeutopnmdhvfklscirqgx\nselpyhdginkxfouq\ndifgepshqlukonx\n\noyz\nyoz\nyoz\nyoz\n\nkohdfzpsj\nrjkphfdzog\njopfsukhbdz\n\ncpvr\ncvy\n\nxavdpsto\nbweoysidhp\nrndplvsukoca\nodgusp\n\nofzgbwqdnkjp\nvcbnlqwofisg\nenbrymdqpogjwf\n\nukrdtiphflv\nfvdlnuirtkh\n\ns\ns\ns\nms\ns\n\nsmodvglcpenzbtw\nvgwcmfdnlobi\ndobmzclwvng\nconvbglwdim\nwrcojghvdblnmx\n\neuy\niey\nhrkyowpesz\nqeuyn\nylfe\n\njd\nygd\nd\nd\n\nifxlcevjr\nmuqybvdntl\n\nhpjdqw\nwjcmqdp\npkojqud\ndjpqm\n\nfz\nz\nz\nauzlgh\nzf\n\noadfixtrspzymgkjvhc\nkureodwxjzvqcagmflt\n\numdbplaejicognthw\nmldwpqitgubajnecho\ngdrhabsmketcwpolunij\n\nutpjbioagl\npujgaobsti\n\nwzqkmfigsenp\nbtdpmnrhaix\n\ng\nh\nfmy\n\nqfdshywubmn\nasnjyumvhfepqwbd\n\ntko\nk\nki\nk\nkvj\n\najrftpclemziyx\ntyfcpmzxjrael\nrlymxejactzpf\nxpfzalrmjteyc\n\nlngatvoimxerfs\nxvsyefntoli\nixvdqstzlonbefk\n\ndytmarichvj\ntvhdurymj\nmefohydrtjpvx\nyajumrtvhd\nmjrvthdy\n\nmdizkcbhuqxfpew\nlciqkhpdzwumfxbe\ndmwbhzqpojuexikcf\n\nav\na\na\na\na\n\nbuwiq\nwbqi\nbqviw\nwbjifuq\n\nszyogteivkjmubnl\ngnmeioyhdjzkutvwqb\n\nvetwlrjnzuxfamsydoqbhpcigk\nlhagxseotdcqbyrwnpfkizmjvu\n\nikt\nsxuik\nrigbk\n\nixzofbapunser\numbfxasovni\noansfubxi\nxsoauibnfk\n\nebzhanm\nrsywcgfuaoqxjvz\n\ndygrvauxpemhosqkwtfji\npxafqktysdjvgriwuehom\nikurtagpdqswmfjvhyxeo\nmhqlyugsravkxoptdwzfjei\nwejrsutgikxqpfvydhaom\n\nynam\nny\nnys\n\nexwzkcjqnvp\nekpzvqxjwacn\nwqnjveazxpkc\nzjvbqkwcrnepx\necnvywqzjsxkp\n\nqmfe\nqmep\n\ndlwymbrsvgxefi\ngxeyfiqmr\n\nmjgkdnbqzcrtfvue\nujgvnrtedqkzfobmc\nnerkmbcuztivgfdpjqy\nbktnacmqvjredfuzg\njgdecrktqnbvmufz\n\ndesyfjcwkgpabomrxluq\nuecwxsrgybalkpqofjm\ngmlaojpwqxsebufrcyk\nsxfoakcyjlmrghqepwub\ngmwouepvzjsirxaylfqbkc\n\nlugbrcezj\nekzuhpxtl\n\nbzurvkdm\ngkzbdvmuwq\n\nvihtwalesoqmgzrfub\ndqogrhlvueimaszbtw\nlmbiwygzaenursotvhq\n\nfagwlnujrsv\nujhnvmcrpsqwl\n\ndsoq\nodzqs\noxehvgqunspa\nsmoqd\ndsroq\n\nlw\nqnlwbze\n\nzcs\ndpalw\noqevmritxf\n\nhw\nwh\nmhz\nh\n\nxwpkgiureobdvamtlcs\nsumvltgxoerdpwbacki\nfaozilpxetcbvwnsrkugdm\ndioulrkpawstcbxgevm\n\ncfziqrvtghejx\nzahecstbvjrqy\ngcqrzvjnteolfhd\n\njronwks\nkjwrso\n\nxtfp\naqwzv\n\ngjvrecbxaqns\nkoficxqpjsvnalzb\nvcjbqxtunsa\nbvjxnqcas\n\nn\nn\nk\nif\n\nhnzquyjxadotim\nctrmnlvfugjkoziqy\nubnfoyqctmzpij\ngyqzvjwimunot\nnzmoluqikjywt\n\nwykxduqrzl\ndrklx\n\nxdr\nxrd\n\nsdrbokn\nrsnbdkio\n\nqespuyziargjdlwnkbv\niaorexbukcmtnwhf\n\ngti\ndit\nitb\n\ntzmwbguraxjcieodnf\ngmitqzdsnekjoxaf\n\nkdeziwsnf\nlgzkiwen\n\nfbvikdpshltegu\nkpfvidlguehtsb\ngdysepktuihlvfb\n\njzegrfyducaospkltvwq\ncrzgkleptvuwsfyoqjda\nrogqtajuweylsdfkzcpv\nzwacrlsptgqfoveydukj\nufokdrzglqjcewtaypvs\n\nhsqnofzvt\nahvpn\n\numaznoshrwf\nhzosundfr\nushfnzor\n\nw\nie\n\nbyandqklvw\nwadvyxbqikn\n\nxspcwfvnkuoydagthjmzbq\nahnprtxeojdmgfqvwzuikscyb\n\ntxpwbvjacrzifgshyluo\nhgytapvojfsbrxwilcuz\nwpvxzshcrabfjtygliou\nbvourjgzfytpiwchlaxs\n\nufesbdavpn\nsiunvefpdab\nnspafhdvbue\nvednpusafb\nsufvpbande\n\nlnzxudorwykfqs\nvckufbqxhwjmtga\n\ntwz\ntwz\ntwz\ntzw\nzwt\n\nnuaeqphyvo\novaqetnphu\nwvhulnpsxroqi\nntvqephou\nhyvuqpon\n\nszky\nkz\nkz\nkz\nkz\n\nuekpcxjqrbzy\newiyrkobtl\n\nrghuyp\nntyzexi\nyjnq\nsy\neysao\n\nyohactxlqbvsfe\nyacfqtxlsvbeho\nqxtaesbfhlovyc\nybotxfqschvela\ntsxbqlcyvfohea\n\ncrvkbef\nefbvkc\nerfvbkc\nfvbikec\necvfkb\n\neziafud\nodfeiz\nfeizd\nrmeflzidc\nzeifdo\n\nuh\nu\nu\nu\n\nslzwuvhqnbpogia\nbhviznawou\nufazbvinhwo\nariwbuvzhon\nvawnobztduhif\n\nlyi\nsyi\nimy\n\nigx\nqfz\n\nyugfqtepvxwdocijlzmhka\nymdleizpjgxcvtfhoakuwq\nutenhymdcgjqzlorxfvakiwp\nzwlvymtgfxeqcopuahjkdi\nauzpqtcifejhomdklyvwgx\n\nxfvdiwqpktjlaborgez\nmlcubagjpwzynxekqfiotd\nfadoezjwqxtpgbkli\nosldxpzbqeifajtkgw\n\nmfxjtn\npqesvmxf\nxmbojhft\nxbmafc\nacbfxhm\n\nbsua\nuasb\n\ndtmpiwjkuahzryfvqgx\niavzqyhgmrxwtdukjp\nthzkaxwjpydqmviurg\n\nuabgmyoidlekzcqnfvxsrw\nkcybroagdmufqxiseznvl\neroscxuqvglanbikzyfdm\ngfmdeibnkvuaqryzloxcs\n\nwtmds\ncxm\nwqdrmz\nvoahgpkfml\n\nqmougypvzh\nyoqsgdpnmhuz\ngauxyzpkoqmjbwfl\nqguzoivyrdmpt\n\nuyerotgj\nwqszinpyv\n\nrosx\nors\nors\nosr\n\ndtcuiwlse\nbgaosqntxuwfpvrc\nuymskctjwz\n\ncshbnzoduxlpfrayj\nhcasypofnrxblduj\npjyuoxrfbsaldhnc\nuharfpoxcdysbnlj\n\nbsklgcdmpxio\nojbpdlcmtugskx\nexdkmpboclgy\naipogmwxdkrclb\ndilmqcpkxnwbgo\n\nxsajoug\npxujgatso\nguxojsa\nmbxuwzjosga\npxjsugoa\n\ntgfw\nxemdgvyfu\njfga\nlfg\n\nkhvg\nkghv\nvghk\nghkv\nvgkh\n\nxfogcwnvkey\nkvxwomyfeqc\nwoxycvafkbe\nyovafwxcke\nekvfyxnwco\n\nezpfkoxycuwgimqvabdtjnsrh\nnmpoxgwdyruecazsfjbhqktiv\nqtjzmaycsgnibdxeuwrfkhvop\nyfaqredhbxinsvucogwtkmpzjl\n\nmhlnsrfadjpekczwquvo\nvsdneluworhbqjfztamcp\nsmthfdwjznvqpclogaure\ncevkfuzqxjmwrlasphnod\nijrnwqpvudzalfoesmch\n\nicmdtnrpq\ntmdqnirp\nmdintrqp\nrtpmniqd\n\nclspvk\nxclpsw\n\nrmdoajtxvc\nvurmjotdsa\nmdrovatj\nnadvmrjlot\ndrjotavm\n\nkl\nhfjkblv\ncklr\nlrsk\nglk\n\nxpoawenkc\nyrkpnzjocxue\npskncodex\n\nb\nb\nbq\nbh\nb\n\nkngbuqcyijvxeh\nkbcmngjshaxiyve\nenybjkvqtucgixh\nzhrevfodplyxnwikjgbc\n\nvizojfmup\nmfzhjqpuoiv\nuvtmfzipjgol\n\nysobmzkthjdw\nqlncxwamrtujevfpzgbhkdo\n\ngz\ngd\nickxr\n\nbrenfplc\nrzpvwumik\nrjctp\n\nhfwcruyoa\nihyduxfw\n\nzlumthynfkjixo\nkgxnbhztfyojmli\nkifzxhtlymngoj\n\nrqpdbzgjsowakvt\ngdzoyhjetcfbqkl\nnbjgqftozkud\nzxbogjqktd\n\nwritbe\netri\ndtfkern\n\nctwyqdi\nj\nme\nnkgzau\nsxkare\n\nqlwakzuxsnfjc\nufzkhmwax\n\nnjuysxmhdzivfgqb\nuamlqxdhnyvgjibfz\n\neigl\nlige\nelig\ngeli\nlegi\n\nvrmhebiqupxoclfatdkg\nytaoiknwcgrsdzevxj\n\nrhac\ndcajbr\noxizsuykfvlwn\n\na\nna\na\nnka\nzdua\n\nzedtlhmj\nyqoxn\n\nwyqhegrp\nwypqhegdr\nunrypgwqhez\nhypqrgbwe\nyrgpeqhw\n\nmxjkhbqgwuniftedzvcl\njciqfnbeumlkhgtvzxwd\nnjwquxidvbhctzmklefg\nhqfwtcmkuxvbjdelgzni\n\nlsigyrmjndapwtehux\nmtwpirehgfxuasdjnoly\nhpdlgjnimeusbtyawxr\nagywjlsmktzihxuceprdn\nnjwhigapyexmdlutvrsb\n\nwiaejbsdzncytol\ncjelwafyindtbzos\nzalobneisywctdj\n\njbsipuwdcyth\nthbauysdcgpiw\n\nbw\nb\no\nlr\n\nojkmnthfzlvg\nvhpzmgtonw\nyjovmnfwthz\nfnzhtmoav\nzcosivnmhdetx\n\nhdkofir\nkanxoflugziyd\nkodfcrwi\n\nqxpmckuelwtarij\nspxmvqwjrileckuat\ntuiqrajpwlmekxc\ntaeupwljxricqmk\n\ndn\ndn\nnd\npnd\n\nfvyxadu\nfwxmny\n\nvsxgc\nvscxg\nscgvx\n\nwovudbian\ndyeosauvinbw\nauvdwinbyo\nuaxoqnwirbvzc\n\naosvcdn\nosna\nsano\n\nshqt\nsqth\n\nktlracvqpeju\ntukcwearql\ntrealcukq\n\nrsezcvdbal\nzebvamcdrp\ncbedrvafxzn\nvxnibedzrac\ndbirvczae\n\ngbpzy\nubglkzp\npbgz\n\njtxhnfge\nxgfhevjtwn\ntjhngfxe\n\npvodec\ngoedfvamlx\neyovd\nevdorpn\nvedot\n\nlsyqpjuviohbz\nluzjybidopvx\nyiplobzvj\nzobwtpjlvnyi\nbzoilvpjyu\n\ncevybwgdmuahsqzp\nucpnimw\nowunkcpm\npmwoucf\n\nuqchtxvoaze\nehocxtvza\ncetazvhxo\nztvxcaeoh\n\ncgsonqxtdfwhzymv\nqbkncfmuxapdwl\nmfxjztcwryqdne\n\nzbkyrc\nckrzb\ncrkzb\nzkbrc\n\nmca\ncam\nmca\ncafm\n\nz\nz\nz\nz\n\nzytoupbkxdhmgcsflweai\nsmpikvyzcxlawobhefudgjnt\ndolpihwzxqckmaufytgbse\n\nktsr\nktfs\nitsxvkb\n\nn\ni\naklsuty\n\nwkgtem\nmewgtk\ngkemwt\nemwgtokb\n\nmbuxkiycz\nfhronqjexplgwa\n\nhu\nemlw\n\nvkuwrqnj\nhys\nmzet\n\nfmclnjvsaexpkg\nzjwpxevkmacsglf\nxsamjlvkcenfgp\nvapjkcmglxefs\n\nlegmbjuqsdkihtv\ndlsqkegihbcjtvm\nhqgdmvljsrktebi\nehlbvgidqtjkms\nbtmshvdikljgqe\n\nk\nkr\nlfi\n\nobmup\nubm\n\nazqdrvjimbfnoyp\nzbejcqxfkotplndv\npbojdfsnzvuwq\ngjpounhvqdfsbrz\n\nminxopwrsbvqegyuajcdth\noahnukmgepbtdriscxwvyj\nxmbtcwgrasnozhdjiupeyv\n\njitgzlokacv\nvzkushynclojtxgep\n\nntj\nntj\ntjn\njtn\njnt\n\ngdzxykoialsqurtj\nkigupewoxqjaztcdy\nzugyaqksiojtxd\nhyjqxgzdauktio\nkygqutaszdbxjoi\n\norsfkumexiq\nrsbjepxmi\nsmrxqgeik\narnsetyiomxv\nmwighaxsdrce\n\nmcuzkqvsdpltxe\npklvsuexmgqwizd\nzdqxseckmtyvlwgrp\ndvslkezanoqxmpb\n\ntcbqprkjgnihzs\nivgmxyzaernhwcqdjlt\notqhfjgcunsrzpi\n\nomqbwvktlsxjcfzaphyeid\nzijtkyxhqvowpulmbgenrdca\n\nigrz\nirzg\nrzgi\n\nkefw\nkewa\nakew\nxtekw\newkf\n\nlvxahjydcer\nsobnzgkp\nmusqfg\n\norijnpvwm\nobqzv\nulvo\nozuvt\nzcbolkqv\n\nouzpgsxejbmqkat\nbtiuxyojspgzqalrek\nekbqupzaxfotjgvs\n\nt\ntb\nt\n\nksyegbpm\nslmpgdetzbk\ngbseumpyxk\nekvphnbcjgms\nwgqrmispobkefa\n\ngwexf\nwfxeg\nxwfeg\ngfewhxb\n\noephijmkngbxw\nkxobwjhiegpm\nwehixbgkjmop\n\nrmvtujdxhki\nlkunxhme\nmafxsbhuogkypw\n\nrpimb\nprbi\nipbr\nbrpi\nribp\n\nhvjdeyw\njhecqpbyvsdw\nwvhdjye\nwvjedhy\n\nuodmbcpvr\nwvhk\n\nj\nj\nj\nj\n\ncrzngwqm\nzqgrwnc\nrcgnwqz\n\nxstoyzgvaefqclbuhi\nywbmiguzthefkvqo\npcuezdviyoqfbjght\nubigqfztyohve\nzovpgfueibjtqsyh\n\nqatefihbypn\nxwvskd")