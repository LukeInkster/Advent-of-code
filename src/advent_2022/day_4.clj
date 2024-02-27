(ns advent-2022.day-4
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-input
  [input-string]
  (map (fn [pair-string]
         (map (fn [range-string]
                (let [[start end] (map #(Integer/parseInt %) (string/split range-string #"-"))]
                  (set (range start (inc end)))))
              (string/split pair-string #",")))
       (string/split-lines input-string)))

(defn part-one
  [input-string]
  (->> (parse-input input-string)
       (filter
         (fn [[elf-a elf-b]]
           (or (set/subset? elf-a elf-b)
               (set/subset? elf-b elf-a))))
       (count)))

(defn part-two
  [input-string]
  (->> (parse-input input-string)
       (filter
         (fn [[elf-a elf-b]]
           (not-empty (set/intersection elf-a elf-b))))
       (count)))


(def small-input "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8")


(def large-input "54-59,17-62\n20-93,57-92\n6-54,7-54\n3-99,59-98\n5-8,5-8\n89-94,32-89\n1-91,1-90\n26-90,91-93\n33-76,21-75\n59-60,19-60\n15-94,15-94\n60-62,59-77\n16-26,12-21\n5-10,11-98\n10-97,9-10\n62-92,61-92\n57-74,38-74\n79-85,84-86\n14-86,13-86\n23-27,22-93\n20-99,19-99\n37-62,36-63\n77-95,88-94\n1-1,3-28\n18-19,18-84\n75-94,74-99\n34-52,20-21\n63-95,2-63\n98-98,82-85\n1-89,2-90\n18-99,22-91\n26-43,27-43\n40-97,41-96\n43-94,88-97\n35-79,11-36\n10-63,10-88\n54-69,53-70\n4-53,2-52\n40-77,77-77\n17-84,79-84\n4-45,5-46\n6-43,7-33\n26-81,26-81\n12-76,11-65\n47-94,47-84\n27-99,26-99\n81-87,80-87\n16-30,1-15\n23-77,23-77\n35-43,36-36\n3-90,17-90\n16-50,15-18\n13-37,37-69\n45-45,44-52\n28-70,28-70\n61-68,61-69\n32-94,32-93\n48-48,31-51\n33-35,34-49\n27-38,38-39\n11-37,11-14\n4-49,3-49\n16-73,72-73\n16-98,22-78\n5-92,91-91\n11-96,10-96\n67-86,82-95\n28-91,92-99\n5-9,5-25\n7-98,6-30\n2-79,11-78\n13-85,85-85\n6-99,98-98\n24-97,68-97\n4-39,3-39\n24-82,57-82\n3-99,1-99\n52-90,90-91\n69-70,69-70\n62-83,61-61\n71-72,17-71\n5-95,4-94\n50-70,46-48\n33-34,34-96\n29-84,29-98\n28-88,27-87\n20-96,21-96\n21-45,46-68\n54-87,54-97\n10-22,10-22\n75-87,15-76\n15-36,14-99\n10-99,9-92\n28-89,29-84\n8-71,7-72\n36-87,36-88\n76-80,46-82\n36-49,7-36\n38-40,40-61\n94-99,28-93\n15-69,54-69\n31-53,52-52\n69-71,5-70\n27-83,3-84\n6-59,59-60\n34-43,33-79\n3-97,4-88\n13-47,13-47\n3-11,10-42\n13-83,12-83\n63-63,62-80\n40-60,60-70\n6-96,7-96\n6-40,21-40\n30-43,27-42\n4-11,21-69\n96-99,5-97\n2-92,2-91\n22-67,23-52\n17-94,17-27\n25-25,24-93\n18-86,18-71\n32-38,38-76\n45-91,45-83\n72-72,72-72\n34-64,64-76\n64-95,43-96\n19-91,2-90\n38-85,84-84\n4-68,3-5\n7-9,8-20\n12-12,12-97\n43-76,75-76\n49-72,71-71\n84-88,84-88\n24-64,23-63\n27-79,11-27\n8-85,5-89\n38-77,6-78\n24-98,25-98\n8-98,25-83\n15-32,5-15\n72-97,7-98\n69-91,70-87\n45-92,5-91\n25-75,76-76\n10-99,10-99\n4-12,10-12\n95-96,64-95\n29-84,29-84\n56-95,94-95\n14-93,11-42\n43-43,3-44\n5-93,47-93\n80-88,79-96\n25-56,16-25\n25-88,26-64\n1-93,2-2\n20-74,20-74\n95-98,96-98\n41-41,40-40\n18-19,12-19\n29-37,36-36\n2-50,3-50\n2-97,3-96\n12-12,11-30\n25-86,26-87\n18-42,18-36\n8-10,9-11\n47-49,27-48\n2-69,69-69\n16-18,17-96\n18-18,17-99\n34-55,34-55\n8-83,88-98\n55-56,46-55\n22-63,11-64\n47-86,32-85\n2-2,1-67\n5-97,6-6\n4-35,1-44\n43-86,54-97\n23-95,23-95\n29-39,29-38\n5-93,5-93\n3-3,3-99\n2-96,22-68\n47-78,48-78\n58-77,58-78\n2-75,2-89\n23-53,17-20\n26-60,9-27\n7-7,6-64\n25-42,15-43\n27-75,37-75\n20-96,21-96\n18-36,35-35\n10-97,10-97\n4-6,9-68\n12-83,13-13\n43-48,26-44\n43-98,43-99\n40-96,40-40\n1-2,2-82\n14-85,14-85\n11-44,10-45\n71-92,91-92\n23-69,24-64\n75-75,11-76\n14-99,2-99\n67-74,73-73\n12-85,13-85\n39-80,40-40\n33-34,32-34\n58-59,13-59\n83-88,84-84\n17-24,9-70\n4-87,1-99\n1-48,2-94\n14-95,10-89\n31-43,31-92\n67-81,49-72\n15-74,16-16\n29-66,29-66\n30-98,97-97\n3-75,3-76\n40-41,26-40\n68-68,67-78\n23-53,24-90\n51-55,36-58\n3-97,4-93\n8-90,8-48\n9-64,8-95\n17-48,47-48\n19-69,46-68\n21-96,21-96\n41-41,37-42\n2-98,97-99\n13-13,13-13\n21-82,38-82\n6-82,81-81\n2-38,37-37\n26-93,5-93\n1-45,2-28\n3-70,8-63\n48-54,51-54\n3-8,7-41\n17-76,16-77\n40-44,40-43\n57-86,27-57\n27-87,24-71\n81-93,92-93\n27-46,46-46\n4-94,7-93\n48-85,84-85\n61-63,13-62\n11-12,11-22\n33-86,33-34\n17-34,18-34\n8-30,31-34\n6-91,6-90\n21-55,21-55\n15-48,25-35\n28-29,30-53\n48-81,47-83\n12-26,11-26\n89-89,22-89\n20-98,21-98\n5-97,76-81\n3-76,3-76\n17-79,79-90\n20-75,28-74\n9-58,8-59\n55-57,56-72\n52-58,45-49\n6-97,21-86\n8-74,8-74\n48-95,75-94\n1-87,8-88\n40-49,36-49\n8-72,8-73\n2-76,3-76\n22-33,2-33\n46-94,18-94\n13-98,7-98\n52-66,14-65\n7-8,8-8\n34-35,34-72\n8-21,20-86\n96-97,2-97\n60-60,59-89\n4-11,10-96\n14-43,14-43\n31-44,43-79\n36-78,35-78\n70-91,67-91\n65-66,43-65\n3-6,5-96\n27-50,9-35\n27-28,13-28\n73-82,72-81\n2-43,2-42\n76-77,75-79\n88-89,23-89\n5-5,4-68\n6-6,5-98\n21-22,9-22\n6-94,5-6\n26-89,20-88\n39-93,13-95\n5-93,22-93\n8-23,9-23\n30-52,31-51\n55-55,6-55\n26-83,26-83\n52-72,51-78\n25-28,41-87\n35-98,36-64\n96-97,79-97\n56-81,57-79\n40-42,17-41\n30-82,30-81\n6-14,9-15\n11-29,11-30\n8-41,6-40\n28-97,97-97\n33-67,33-68\n29-90,28-91\n2-30,12-29\n5-70,6-6\n8-39,9-79\n5-29,28-35\n17-55,17-55\n3-90,2-90\n15-30,15-72\n77-78,16-78\n60-82,36-83\n2-91,4-89\n37-52,52-52\n11-98,10-98\n55-56,24-55\n27-59,25-59\n2-2,1-93\n11-49,49-50\n23-71,23-86\n17-52,3-51\n98-98,21-99\n3-92,4-4\n81-85,81-85\n17-99,18-95\n5-90,1-2\n33-78,33-77\n34-47,20-47\n1-97,35-98\n95-98,34-96\n80-92,94-98\n63-70,69-70\n89-94,90-95\n38-38,39-81\n74-75,33-75\n16-33,25-97\n29-97,29-29\n28-30,29-93\n26-75,74-75\n98-99,1-98\n37-60,32-46\n18-36,17-88\n29-95,29-95\n16-32,32-33\n36-37,19-38\n36-84,33-42\n56-68,6-69\n14-57,42-57\n68-76,67-73\n14-24,23-37\n59-60,60-87\n16-98,15-98\n46-47,43-72\n97-97,64-97\n45-65,64-64\n35-98,12-35\n1-99,1-98\n7-77,8-76\n7-89,8-56\n65-65,54-68\n19-94,13-95\n86-90,85-88\n14-79,13-80\n15-15,15-77\n6-68,6-68\n6-6,5-60\n50-89,37-90\n7-66,7-67\n20-38,20-37\n45-92,46-97\n77-78,67-77\n3-3,2-99\n3-91,3-4\n4-84,25-84\n10-11,10-66\n4-45,4-45\n33-62,25-34\n38-67,39-39\n15-70,63-64\n58-75,57-74\n6-98,3-7\n22-54,47-53\n20-45,32-46\n1-91,1-90\n3-97,99-99\n1-45,1-1\n3-92,28-91\n5-83,5-84\n5-98,20-98\n51-52,51-52\n15-93,12-23\n18-21,21-89\n89-92,88-95\n8-27,12-54\n12-28,13-28\n25-27,26-27\n63-64,63-70\n1-75,1-25\n13-41,14-14\n15-30,30-93\n9-89,9-90\n14-70,15-70\n11-12,11-52\n3-3,2-98\n8-10,9-91\n67-68,57-68\n7-33,8-94\n33-90,2-89\n13-57,12-58\n62-91,73-74\n31-99,32-77\n4-19,1-3\n26-35,34-42\n19-64,18-55\n51-64,87-91\n3-99,98-99\n4-45,8-92\n53-86,85-86\n22-84,22-79\n17-79,80-86\n37-37,36-82\n7-83,7-82\n94-95,78-95\n93-94,49-93\n70-82,18-98\n4-85,3-84\n31-84,30-71\n16-32,24-31\n10-91,4-63\n11-94,11-92\n70-71,62-70\n43-83,60-82\n10-47,6-46\n12-58,57-58\n10-64,10-56\n24-95,42-94\n1-38,18-37\n19-53,20-53\n26-37,26-37\n76-80,77-81\n10-16,16-16\n24-98,40-79\n26-92,27-92\n14-99,14-15\n1-98,96-96\n85-99,84-96\n34-60,14-61\n40-82,81-85\n1-99,1-21\n20-28,20-29\n4-73,1-73\n2-2,3-93\n55-97,7-98\n18-66,19-74\n78-80,80-80\n9-80,26-81\n26-97,4-99\n14-87,13-88\n81-82,82-86\n29-33,30-35\n81-89,13-82\n33-42,34-68\n27-94,26-95\n5-93,93-94\n8-86,64-87\n5-74,4-75\n66-90,94-97\n58-61,58-60\n52-90,50-58\n22-22,22-22\n53-61,53-54\n9-66,65-77\n42-60,43-43\n68-97,68-97\n16-89,15-90\n44-96,95-97\n12-89,13-88\n35-52,34-52\n45-78,46-86\n7-24,7-20\n6-53,5-54\n57-58,14-57\n73-82,77-81\n43-50,44-50\n32-48,32-70\n91-92,29-91\n4-86,70-87\n91-92,45-90\n34-98,35-35\n34-98,90-97\n24-96,23-97\n30-62,29-62\n37-37,36-66\n20-91,49-90\n85-94,19-99\n37-77,10-36\n1-46,45-72\n50-88,19-88\n87-89,19-98\n3-91,2-97\n10-75,10-75\n4-96,3-97\n50-85,22-50\n68-94,69-92\n7-66,7-8\n18-72,25-73\n13-52,51-52\n84-87,2-87\n4-86,87-93\n45-49,49-56\n4-25,3-26\n26-55,24-27\n25-47,26-26\n45-56,44-56\n61-64,60-64\n33-96,34-95\n49-50,26-51\n3-94,4-94\n8-97,8-95\n6-64,5-35\n21-31,22-32\n33-73,11-72\n14-75,11-11\n97-97,5-98\n28-29,29-30\n90-99,23-47\n83-84,1-83\n3-92,2-4\n3-4,4-96\n96-97,28-91\n23-52,22-53\n67-67,66-68\n23-91,22-92\n2-99,2-99\n3-41,2-94\n36-87,59-86\n69-77,69-77\n9-99,8-99\n4-96,3-96\n52-52,21-51\n19-89,18-53\n86-87,29-87\n18-65,25-54\n62-82,4-61\n49-94,49-93\n95-98,5-96\n22-87,22-49\n98-98,41-99\n8-93,7-96\n67-72,67-73\n29-62,50-78\n4-95,95-96\n29-90,16-89\n2-75,74-74\n54-56,30-57\n1-95,1-95\n7-98,8-8\n83-90,40-92\n50-93,49-94\n34-47,46-47\n72-87,72-75\n26-69,27-69\n15-58,14-16\n14-63,17-99\n9-87,9-86\n1-99,3-97\n45-51,34-63\n24-43,13-13\n5-20,4-21\n5-75,6-6\n99-99,8-97\n12-40,11-85\n26-85,27-84\n5-86,6-6\n22-82,22-81\n42-94,43-75\n6-7,6-96\n5-92,4-92\n32-37,32-38\n64-87,65-82\n58-63,60-60\n15-97,15-91\n10-17,16-45\n70-71,49-71\n2-34,29-35\n7-81,6-7\n36-88,37-54\n81-86,76-87\n2-93,2-5\n33-89,88-88\n25-37,18-36\n11-56,12-55\n8-91,7-95\n5-83,6-66\n65-97,19-66\n60-81,80-82\n23-94,1-93\n42-91,41-92\n79-94,79-93\n3-99,2-17\n75-75,24-76\n16-61,16-76\n33-75,34-74\n32-88,87-88\n4-26,24-27\n36-46,40-50\n15-76,5-11\n2-12,3-12\n3-97,4-92\n9-21,20-20\n9-95,66-94\n14-92,14-25\n33-49,20-48\n31-67,35-68\n1-97,96-96\n35-35,34-92\n57-86,56-72\n68-85,53-86\n80-87,67-89\n94-94,5-94\n65-84,84-91\n11-99,7-9\n20-92,21-82\n3-61,1-3\n34-59,33-58\n12-98,13-13\n80-80,13-80\n55-97,54-98\n59-76,3-77\n8-97,7-74\n45-46,43-45\n60-81,9-82\n4-88,25-87\n5-97,96-96\n39-70,25-51\n44-61,41-42\n33-36,33-34\n91-92,6-91\n39-53,43-53\n9-31,30-32\n60-96,59-98\n17-98,17-17\n41-41,40-43\n16-44,35-44\n42-42,14-43\n1-5,5-98\n18-87,33-87\n56-56,46-57\n6-99,2-98\n9-22,10-10\n14-15,15-61\n17-82,18-34\n95-98,14-95\n81-81,38-82\n30-50,39-46\n94-94,6-95\n14-54,15-53\n3-80,2-4\n55-55,54-98\n99-99,2-99\n70-70,9-71\n8-20,9-9\n1-98,98-99\n31-71,63-71\n74-97,25-97\n4-61,61-61\n37-58,38-53\n15-20,14-15\n44-79,45-80\n45-68,29-75\n24-94,19-98\n42-86,9-41\n75-89,70-75\n25-55,6-25\n41-69,40-41\n6-96,4-4\n20-94,93-94\n16-69,2-68\n76-78,47-77\n7-54,3-36\n2-38,1-98\n4-16,16-16\n1-99,88-99\n12-35,15-35\n14-98,13-99\n60-61,8-61\n26-68,58-67\n28-98,28-95\n13-15,14-30\n23-41,24-24\n33-88,14-81\n36-94,3-67\n16-74,73-74\n83-93,3-94\n3-65,6-66\n31-93,32-32\n30-54,41-54\n9-77,76-96\n11-12,11-74\n14-14,19-72\n34-60,33-61\n3-4,3-25\n3-75,75-76\n28-99,29-97\n29-85,30-84\n21-31,24-29\n10-35,10-11\n92-98,56-93\n3-96,95-96\n36-55,36-90\n4-91,5-91\n1-93,92-98\n21-37,22-36\n9-32,30-33\n46-83,47-89\n31-63,30-32\n28-54,58-66\n33-97,33-97\n71-80,59-79\n11-97,12-98\n68-69,66-70\n9-62,6-46\n50-92,50-91\n5-19,18-77\n23-25,19-25\n2-11,3-15\n11-92,12-92\n21-99,22-87\n57-92,56-93\n12-91,2-9\n6-13,1-14\n8-92,9-91\n7-38,4-71\n2-11,2-80\n6-99,6-99\n23-56,55-65\n44-78,54-77\n47-63,48-63\n9-81,8-31\n67-84,67-87\n2-74,3-73\n3-11,10-97\n18-20,21-99\n92-92,34-93\n44-95,33-94\n4-61,14-61\n4-97,1-97\n79-88,80-90\n13-93,10-97\n16-44,15-44\n47-90,8-75\n17-59,8-60\n11-87,4-11\n4-64,4-69\n66-67,67-91\n48-67,49-66\n26-50,25-51\n4-91,4-91\n60-60,59-83\n1-1,1-16\n4-15,16-16\n68-72,67-68\n42-84,41-85\n22-73,18-72\n50-51,51-78\n6-98,6-97\n72-83,72-86\n1-98,1-97\n6-28,6-79\n27-82,81-82\n3-72,2-3\n2-97,6-66\n9-92,8-44\n3-6,5-65\n85-86,30-86\n75-97,8-69\n5-60,6-6\n9-64,8-76\n36-88,41-92\n1-57,57-57\n76-92,75-93\n18-85,2-18\n18-68,14-31\n48-49,11-48\n86-86,15-87\n41-93,42-92\n9-40,10-39\n5-6,6-86\n2-38,24-84\n16-90,7-17\n19-63,18-64\n6-97,78-99\n19-48,20-93\n6-8,7-55\n3-74,2-73\n5-22,6-6\n6-84,6-92\n30-30,30-82\n17-37,16-37\n82-89,5-98\n49-98,97-98\n6-19,18-20\n6-95,5-95\n10-46,29-45\n5-84,6-83\n97-97,21-98\n57-83,58-82\n21-59,45-60\n2-98,1-99\n52-56,9-51\n71-86,44-87\n3-81,2-82\n53-94,80-93\n1-83,2-84\n31-32,31-84\n79-87,80-87\n77-79,6-78\n27-88,18-73\n77-93,6-94\n76-76,8-76\n36-38,34-39\n20-37,9-20\n33-34,34-99\n22-98,98-99\n97-98,5-98\n1-40,2-40\n62-93,62-93\n47-94,48-94\n5-89,1-5\n5-62,23-61\n7-60,8-59\n88-89,43-88\n3-95,94-95\n18-31,6-31\n60-73,48-74\n5-56,5-56\n33-78,34-55\n33-37,34-36\n2-68,2-68\n80-82,13-84\n73-73,63-73\n54-70,53-96\n7-16,8-48\n39-79,39-80\n12-37,13-67\n4-72,5-71\n77-88,34-78\n40-74,40-74\n97-98,97-97\n31-92,32-32\n37-61,28-61\n1-91,2-91\n31-58,4-13\n27-80,60-80\n33-94,33-94\n67-72,13-90\n31-51,30-52\n18-90,18-90\n33-43,33-44\n23-98,33-97\n13-45,12-49\n89-90,54-90\n4-60,4-59\n5-92,6-85\n19-23,9-22\n8-96,1-8\n37-87,23-37\n30-62,31-61\n29-79,29-79\n27-96,23-95\n5-99,6-97\n92-93,4-93\n13-36,35-78\n32-48,32-47\n17-96,99-99\n4-98,98-98\n17-17,17-92\n79-84,11-85\n34-95,33-95\n10-79,8-80\n7-32,27-27\n3-98,2-99\n3-63,2-64\n22-98,13-21\n9-20,19-96\n62-97,60-74\n14-86,13-14\n15-90,15-91\n11-92,10-89\n93-98,7-94\n1-75,8-63\n3-92,4-59\n54-72,72-72\n52-84,84-89\n55-55,55-72\n5-5,4-52\n21-84,66-75\n13-18,7-87\n82-83,6-82\n19-66,11-12\n69-96,69-95\n3-98,36-97\n12-31,6-12\n21-49,14-18\n35-97,8-97\n16-82,81-82\n4-92,4-99\n68-79,19-67\n36-56,77-83\n46-47,29-46\n91-92,21-91\n2-2,1-90\n61-93,39-74\n6-33,33-33\n7-95,2-7\n59-60,53-60\n7-90,4-8\n94-94,84-95\n69-85,68-94\n9-73,9-73\n47-99,48-99\n7-37,7-7\n3-3,2-74\n18-18,17-28\n25-51,46-50\n1-98,1-99\n48-80,49-56\n16-94,93-95\n59-83,60-82\n16-79,78-78\n24-26,29-85\n10-77,10-11\n4-96,4-48\n44-97,43-87\n33-71,70-71\n27-57,26-57\n30-53,16-31\n15-68,14-69\n14-95,7-94\n82-82,15-81\n22-40,22-23\n6-94,53-65\n76-95,77-83\n12-83,13-82\n41-94,40-98\n37-58,38-58\n7-91,1-6\n23-68,34-67\n27-97,96-96\n56-57,56-57\n1-61,3-7\n28-55,27-55\n32-32,31-56\n13-76,13-76\n10-18,11-56\n5-99,2-5\n7-80,14-39\n16-82,81-82\n4-24,2-4\n2-97,97-98\n24-88,87-88\n28-95,29-96\n56-72,57-71\n71-73,1-72\n52-98,51-98\n4-57,57-58\n82-83,40-82\n54-78,29-77\n10-25,25-26\n46-89,87-88\n59-89,68-89\n84-86,57-84\n24-94,54-93\n41-63,40-63\n87-90,89-90")