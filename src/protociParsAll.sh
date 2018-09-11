#!/bin/bash

pp1arr=(6 9 12 15 18 21 24 27 30 33 36 39 48 51 54 57 60 63 66 69 72 75 78 81 85 90 92)
pp2arr=(4 7 10 13 16 19 22 25 28 31 34 37 46 49 52 55 58 61 64 67 70 73 76 79 86 88 89 93)
pp3arr=(5 8 11 14 17 20 23 26 29 32 35 38 47 50 53 56 59 62 65 68 71 74 77 80 87 91 94)
pp4arr=(100 103 106 109 112 115 118 121 124 127 130 133 136 142 145 148 151 154 157 160 163 166 169 172 175 178 3 45 179 185)
pp5arr=(99 102 105 108 111 114 117 120 123 126 141 144 147 150 153 156 159 162 165 168 180 186)
pp6arr=(95 96 98 101 104 107 110 113 116 119 122 125 128 131 134 137 138 140 143 146 149 152 155 158 161 164 167 170 173 176 181 183 184 188 1 43 215 216 243 244)
pp7arr=(129 132 135 171 174 177 2 44 182 187)
s0arr=(189)
pdgarr=(190 191 195 199 203 207 211 219 223 227 231 235 239 193 197 201 205 209 213 221 225 229 233 237 241 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270 271 272 273 274 284 287)
pmgarr=(192 196 200 204 208 212 220 224 228 232 236 240 286)
pdv1arr=(194 198 202 222 249 275 278 281 226 230)
pdv2arr=(206 210 234 238 247 248 276 279 282)
pdv3arr=(214 242 246 277 280 283)
psgarr=(285)
b1d6arr=(254)
b7d12arr=(255)
b13d18arr=(256)
b19arr=(257)
b20arr=(258)
b21arr=(259)
b22arr=(260)
b23arr=(261)
b24arr=(262)
b25arr=(263)
b26arr=(264)
b27arr=(265)
b28arr=(266)
b29arr=(267)
b30arr=(268)
b31arr=(269)
b32arr=(270)
b33arr=(271)
b34arr=(272)
b35arr=(273)
b36arr=(274)

listID=(`awk '{ print $1 }' PROTOK | head -n 290 | tail -n 288`)
listIzProt=(`awk '{ print $3 }' PROTOK | head -n 290 | tail -n 288`)
counter=0
pp1=0
pp2=0
pp3=0
pp4=0
pp5=0
pp6=0
pp7=0
s0=0
pdg=0
pmg=0
pdv1=0
pdv2=0
pdv3=0
psg=0
b1d6=0
b7d12=0
b13d18=0
b19=0
b20=0
b21=0
b22=0
b23=0
b24=0
b25=0
b26=0
b27=0
b28=0
b29=0
b30=0
b31=0
b32=0
b33=0
b34=0
b35=0
b36=0

while true
do
    if [ $counter -ge 288 ]
    then
        break
    fi
    ID=${listID[$((counter))]}
    izlazniProtok=${listIzProt[$((counter))]}
    
    if echo ${pp1arr[@]} | grep -q -w $ID
    then
        pp1=$(perl -e "printf '%.4e', $pp1+$izlazniProtok")
    fi
    
    if echo ${pp2arr[@]} | grep -q -w $ID
    then
        pp2=$(perl -e "printf '%.4e', $pp2+$izlazniProtok")
    fi
    
    if echo ${pp3arr[@]} | grep -q -w $ID
    then
        pp3=$(perl -e "printf '%.4e', $pp3+$izlazniProtok")
    fi
    
    if echo ${pp4arr[@]} | grep -q -w $ID
    then
        pp4=$(perl -e "printf '%.4e', $pp4+$izlazniProtok")
    fi
    
    if echo ${pp5arr[@]} | grep -q -w $ID
    then
        pp5=$(perl -e "printf '%.4e', $pp5+$izlazniProtok")
    fi
    
    if echo ${pp6arr[@]} | grep -q -w $ID
    then
        pp6=$(perl -e "printf '%.4e', $pp6+$izlazniProtok")
    fi
    
    if echo ${pp7arr[@]} | grep -q -w $ID
    then
        pp7=$(perl -e "printf '%.4e', $pp7+$izlazniProtok")
    fi
    if echo ${s0arr[@]} | grep -q -w $ID
    then
        s0=$(perl -e "printf '%.4e', $s0+$izlazniProtok")
    fi
    
    if echo ${pdgarr[@]} | grep -q -w $ID
    then
        pdg=$(perl -e "printf '%.4e', $pdg+$izlazniProtok")
    fi
    
    if echo ${pmgarr[@]} | grep -q -w $ID
    then
        pmg=$(perl -e "printf '%.4e', $pmg+$izlazniProtok")
    fi
    
    if echo ${pdv1arr[@]} | grep -q -w $ID
    then
        pdv1=$(perl -e "printf '%.4e', $pdv1+$izlazniProtok")
    fi
    
    if echo ${pdv2arr[@]} | grep -q -w $ID
    then
        pdv2=$(perl -e "printf '%.4e', $pdv2+$izlazniProtok")
    fi
    if echo ${pdv3arr[@]} | grep -q -w $ID
    then
        pdv3=$(perl -e "printf '%.4e', $pdv3+$izlazniProtok")
    fi
    if echo ${psgarr[@]} | grep -q -w $ID
    then
        psg=$(perl -e "printf '%.4e', $psg+$izlazniProtok")
    fi
    if echo ${b1d6arr[@]} | grep -q -w $ID
    then
        b1d6=$(perl -e "printf '%.4e', $b1d6+$izlazniProtok")
    fi
    if echo ${b7d12arr[@]} | grep -q -w $ID
    then
        b7d12=$(perl -e "printf '%.4e', $b7d12+$izlazniProtok")
    fi
    if echo ${b13d18arr[@]} | grep -q -w $ID
    then
        b13d18=$(perl -e "printf '%.4e', $b13d18+$izlazniProtok")
    fi
    if echo ${b19arr[@]} | grep -q -w $ID
    then
        b19=$(perl -e "printf '%.4e', $b19+$izlazniProtok")
    fi
    if echo ${b20arr[@]} | grep -q -w $ID
    then
        b20=$(perl -e "printf '%.4e', $b20+$izlazniProtok")
    fi
    if echo ${b21arr[@]} | grep -q -w $ID
    then
        b21=$(perl -e "printf '%.4e', $b21+$izlazniProtok")
    fi
    if echo ${b22arr[@]} | grep -q -w $ID
    then
        b22=$(perl -e "printf '%.4e', $b22+$izlazniProtok")
    fi
    if echo ${b23arr[@]} | grep -q -w $ID
    then
        b23=$(perl -e "printf '%.4e', $b23+$izlazniProtok")
    fi
    if echo ${b24arr[@]} | grep -q -w $ID
    then
        b24=$(perl -e "printf '%.4e', $b24+$izlazniProtok")
    fi
    if echo ${b25arr[@]} | grep -q -w $ID
    then
        b25=$(perl -e "printf '%.4e', $b25+$izlazniProtok")
    fi
    if echo ${b26arr[@]} | grep -q -w $ID
    then
        b26=$(perl -e "printf '%.4e', $b26+$izlazniProtok")
    fi
    if echo ${b27arr[@]} | grep -q -w $ID
    then
        b27=$(perl -e "printf '%.4e', $b27+$izlazniProtok")
    fi
    if echo ${b28arr[@]} | grep -q -w $ID
    then
        b28=$(perl -e "printf '%.4e', $b28+$izlazniProtok")
    fi
    if echo ${b29arr[@]} | grep -q -w $ID
    then
        b29=$(perl -e "printf '%.4e', $b29+$izlazniProtok")
    fi
    if echo ${b30arr[@]} | grep -q -w $ID
    then
        b30=$(perl -e "printf '%.4e', $b30+$izlazniProtok")
    fi
    if echo ${b31arr[@]} | grep -q -w $ID
    then
        b31=$(perl -e "printf '%.4e', $b31+$izlazniProtok")
    fi
    if echo ${b32arr[@]} | grep -q -w $ID
    then
        b32=$(perl -e "printf '%.4e', $b32+$izlazniProtok")
    fi
    if echo ${b33arr[@]} | grep -q -w $ID
    then
        b33=$(perl -e "printf '%.4e', $b33+$izlazniProtok")
    fi
    if echo ${b34arr[@]} | grep -q -w $ID
    then
        b34=$(perl -e "printf '%.4e', $b34+$izlazniProtok")
    fi
    if echo ${b35arr[@]} | grep -q -w $ID
    then
        b35=$(perl -e "printf '%.4e', $b35+$izlazniProtok")
    fi
    if echo ${b36arr[@]} | grep -q -w $ID
    then
        b36=$(perl -e "printf '%.4e', $b36+$izlazniProtok")
    fi
    counter=$((counter+1))
done

echo "pp1="$pp1
echo "pp2="$pp2
echo "pp3'="$pp3
echo "pp4="$pp4
echo "pp5="$pp5
echo "pp6="$pp6
echo "pp7="$pp7
echo "s0= "$s0
echo "p-dg="$pdg
echo "p-mg="$pmg
echo "p-dv-1="$pdv1
echo "p-dv-2="$pdv2
echo "p-dv-3="$pdv3
echo "p-sg="$psg
echo "b1-6="$b1d6
echo "b7-12="$b7d12
echo "b13-18="$b13d18
echo "b19="$b19
echo "b20="$b20
echo "b21="$b21
echo "b22="$b22
echo "b23="$b23
echo "b24="$b24
echo "b25="$b25
echo "b26="$b26
echo "b27="$b27
echo "b28="$b28
echo "b29="$b29
echo "b30="$b30
echo "b31="$b31
echo "b32="$b32
echo "b33="$b33
echo "b34="$b34
echo "b35="$b35
echo "b36="$b36

echo "pp1",$pp1 > proticajPAK.csv
echo "pp2",$pp2 >> proticajPAK.csv
echo "pp3'",$pp3 >> proticajPAK.csv
echo "pp4",$pp4 >> proticajPAK.csv
echo "pp5",$pp5 >> proticajPAK.csv
echo "pp6",$pp6 >> proticajPAK.csv
echo "pp7",$pp7 >> proticajPAK.csv
echo "s0",$s0 >> proticajPAK.csv
echo "p-dg="$pdg >> proticajPAK.csv
echo "p-mg="$pmg >> proticajPAK.csv
echo "p-dv-1="$pdv1 >> proticajPAK.csv
echo "p-dv-2="$pdv2 >> proticajPAK.csv
echo "p-dv-3="$pdv3 >> proticajPAK.csv
echo "p-sg="$psg >> proticajPAK.csv
echo "b1-6="$b1d6 >> proticajPAK.csv
echo "b7-12="$b7d12 >> proticajPAK.csv
echo "b13-18="$b13d18 >> proticajPAK.csv
echo "b19="$b19 >> proticajPAK.csv
echo "b20="$b20 >> proticajPAK.csv
echo "b21="$b21 >> proticajPAK.csv
echo "b22="$b22 >> proticajPAK.csv
echo "b23="$b23 >> proticajPAK.csv
echo "b24="$b24 >> proticajPAK.csv
echo "b25="$b25 >> proticajPAK.csv
echo "b26="$b26 >> proticajPAK.csv
echo "b27="$b27 >> proticajPAK.csv
echo "b28="$b28 >> proticajPAK.csv
echo "b29="$b29 >> proticajPAK.csv
echo "b30="$b30 >> proticajPAK.csv
echo "b31="$b31 >> proticajPAK.csv
echo "b32="$b32 >> proticajPAK.csv
echo "b33="$b33 >> proticajPAK.csv
echo "b34="$b34 >> proticajPAK.csv
echo "b35="$b35 >> proticajPAK.csv
echo "b36="$b36 >> proticajPAK.csv
