(ns advent-2022.day-1
  (:require [clojure.string :as string]))


(defn calories-per-elf
  [all-elves-str]
  (->> (string/split all-elves-str #"\n\n")
       (map (fn [elf-str] (map #(Integer/parseInt %) (string/split elf-str #"\n"))))
       (map (partial reduce +))))

(defn part-one
  [all-elves-str]
  (->> (calories-per-elf all-elves-str)
       (reduce max)))

(defn part-two
  [all-elves-str]
  (->> (calories-per-elf all-elves-str)
       (sort)
       (take-last 3)
       (reduce +)))


(def small-input "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000")


(def large-input "2832\n2108\n3082\n4328\n6843\n5121\n2869\n1366\n2358\n1680\n4980\n1161\n\n8026\n2154\n4242\n1023\n2744\n3162\n4093\n1150\n5397\n2738\n5657\n\n10954\n11208\n8034\n1636\n9430\n9421\n5025\n\n3837\n5938\n3967\n2942\n2727\n3623\n4070\n1748\n1795\n1687\n4023\n6068\n2558\n2570\n4214\n\n9679\n2986\n5168\n16463\n\n5912\n6008\n1259\n5378\n10740\n1642\n2617\n7821\n\n3840\n5265\n2104\n1510\n5477\n2636\n5920\n5939\n5430\n4482\n5603\n4915\n5416\n5897\n\n3932\n9344\n7536\n4925\n10088\n5253\n5625\n6800\n\n3928\n13714\n18995\n3569\n\n2424\n5188\n11199\n6078\n11525\n5155\n3142\n\n2595\n5893\n5590\n6270\n6095\n5279\n3726\n6015\n3985\n5455\n5190\n4897\n6632\n\n5046\n1424\n3629\n6204\n6187\n1149\n4145\n6953\n2958\n6312\n1592\n4614\n2859\n\n5750\n9606\n11706\n2629\n13635\n5611\n\n2162\n6014\n3168\n8052\n10781\n3762\n5653\n\n21918\n14739\n\n2431\n5955\n4810\n7476\n2529\n7985\n6040\n5864\n3459\n3347\n4853\n\n4695\n3911\n5590\n4439\n4874\n2251\n3967\n6895\n5801\n2720\n5125\n5575\n2057\n\n5642\n3714\n5127\n5083\n2925\n5048\n6368\n5883\n2287\n3276\n3326\n2207\n3377\n\n18373\n7075\n12254\n18539\n\n13758\n19556\n23565\n\n6463\n5076\n5429\n3105\n6201\n5959\n6474\n5740\n6997\n7567\n1883\n\n2316\n10793\n2985\n10596\n6910\n11957\n5984\n\n18521\n7234\n7943\n\n8774\n7905\n4375\n1788\n9244\n2259\n4842\n6313\n\n4496\n10446\n6917\n1113\n2509\n1229\n2999\n\n2158\n3221\n1412\n1484\n1768\n2621\n1087\n1152\n3625\n3767\n4859\n1220\n3745\n4374\n3712\n\n4072\n8998\n5778\n4361\n6138\n\n11068\n15488\n17001\n16349\n\n3493\n15148\n14490\n9502\n11101\n\n8952\n2731\n10734\n7581\n4799\n9379\n5405\n10345\n\n5008\n4456\n3824\n3025\n4493\n2369\n1050\n3368\n3266\n4641\n5932\n5774\n1287\n3001\n2934\n\n16879\n20950\n11598\n\n11948\n2932\n1672\n11100\n\n24636\n12299\n18577\n\n14198\n24466\n25429\n\n3539\n5867\n2303\n1372\n3757\n3255\n4606\n4703\n4477\n3515\n5287\n7330\n\n2673\n5936\n6292\n4183\n4963\n6462\n4193\n5588\n5140\n2825\n4784\n3210\n1682\n3746\n\n12296\n15184\n9500\n16390\n\n2816\n6984\n6514\n8120\n8000\n9203\n3716\n\n24745\n15138\n\n4456\n4546\n4903\n2885\n4578\n2588\n2946\n6634\n1414\n1239\n6070\n4651\n2749\n\n3755\n3424\n3633\n2325\n2730\n5526\n6877\n4528\n6567\n2626\n6429\n2464\n\n2159\n6338\n2919\n4039\n3091\n7916\n2833\n4192\n5486\n2220\n5616\n\n1025\n10572\n6790\n3174\n5986\n2465\n9046\n1559\n\n4652\n9609\n4804\n5109\n5301\n9886\n7094\n\n1888\n9582\n9566\n2777\n1234\n\n6231\n7939\n4757\n3026\n11324\n4069\n6366\n\n5078\n6127\n1616\n5894\n3707\n1031\n5544\n4880\n6766\n6411\n2767\n2642\n1197\n\n2954\n1929\n8070\n6836\n7396\n7779\n5059\n5299\n3101\n1108\n3408\n\n23715\n34978\n\n5986\n8393\n3943\n9746\n8118\n6464\n4882\n\n66377\n\n2291\n6079\n3131\n2006\n6250\n3864\n5167\n3150\n3909\n3435\n2679\n6801\n6361\n\n12399\n2412\n2223\n2628\n7269\n\n6446\n2771\n6190\n5186\n5672\n5414\n4361\n1204\n1306\n2448\n1425\n2860\n3235\n2123\n\n6648\n5782\n2695\n6432\n5396\n5628\n5713\n5889\n5862\n1082\n4800\n6132\n\n1804\n6681\n3451\n6039\n6573\n3927\n5420\n1443\n3711\n1906\n5490\n4212\n2221\n\n4960\n3469\n3779\n1215\n3665\n6336\n5876\n6523\n1211\n1051\n6900\n2665\n6116\n\n5650\n7059\n1520\n3284\n10604\n5219\n6087\n\n1385\n3922\n6315\n3317\n2532\n1591\n2191\n6198\n1667\n2358\n4424\n3384\n1452\n1878\n\n8968\n6735\n12578\n11709\n7573\n1140\n\n8445\n2956\n8682\n8315\n3698\n7811\n6875\n2514\n2348\n\n19576\n3115\n13589\n15609\n\n43635\n\n8805\n24653\n4664\n\n6088\n5967\n5407\n3638\n9220\n1047\n2792\n7947\n9559\n\n9635\n5762\n7826\n4419\n12047\n8603\n6995\n\n6103\n7299\n7646\n4602\n5535\n7703\n6427\n6826\n1506\n5826\n8019\n\n5341\n3445\n2020\n4685\n1068\n4974\n5251\n2968\n4358\n4057\n2686\n1122\n2914\n3998\n1247\n\n3096\n1152\n4346\n3685\n1365\n4059\n2774\n4958\n3561\n1036\n4612\n4404\n3419\n4733\n3484\n\n5167\n1180\n1172\n5057\n3974\n6226\n1309\n4009\n5230\n1105\n4158\n2016\n3786\n2513\n\n2940\n1134\n6443\n7340\n5325\n3750\n3839\n7379\n4766\n4173\n\n4547\n11948\n7129\n6433\n7101\n2361\n8366\n\n1507\n6017\n3115\n6258\n6404\n6000\n1869\n5286\n1623\n1410\n3739\n1632\n2433\n\n5846\n6515\n2172\n6728\n6626\n2611\n1583\n2759\n3192\n3197\n1057\n5425\n2712\n\n7224\n2884\n2611\n1784\n3902\n7393\n5299\n2389\n1941\n4128\n5461\n1015\n\n1743\n9456\n8535\n6753\n7773\n10241\n5969\n8666\n\n19734\n2085\n9467\n12261\n\n1778\n7318\n2654\n3489\n3276\n4511\n2533\n3292\n4718\n5173\n2267\n5451\n\n5567\n5103\n1888\n2210\n4582\n1260\n1264\n4326\n5352\n3549\n5226\n1875\n3156\n3571\n\n1380\n7048\n2322\n1694\n4611\n6541\n8334\n2071\n1390\n7593\n\n5343\n4878\n1115\n1400\n8760\n4952\n5444\n4012\n\n3021\n2193\n6671\n1502\n7320\n7557\n4150\n3959\n1818\n8798\n\n19469\n23032\n11831\n\n8257\n5493\n5583\n7627\n3801\n3468\n1742\n\n9306\n4711\n6825\n8310\n2782\n2447\n10195\n4287\n\n10790\n6505\n15999\n15093\n14116\n\n1469\n2435\n4534\n8314\n7762\n2064\n5315\n1109\n4439\n\n28459\n29638\n\n1530\n1274\n5799\n7684\n2594\n6179\n1450\n4017\n5336\n2397\n7630\n\n1467\n5771\n5730\n2600\n3958\n4481\n4994\n2718\n6763\n4521\n6496\n\n3638\n7722\n6498\n3612\n7521\n1310\n6183\n1850\n6394\n4763\n2657\n\n7537\n4591\n2179\n9064\n6970\n2782\n9339\n1265\n7281\n\n3380\n12388\n5521\n1191\n8912\n\n2776\n3427\n3206\n2075\n2589\n3545\n1806\n2603\n4079\n2700\n3430\n1185\n5708\n1322\n\n1466\n4217\n3529\n1232\n5654\n2061\n5435\n5875\n4521\n4841\n6114\n5252\n1185\n5842\n\n21001\n8876\n\n5131\n5556\n6014\n7791\n3957\n8731\n8053\n8015\n\n10379\n3070\n21154\n\n3012\n5818\n1144\n6315\n4495\n1846\n7923\n1340\n2675\n4955\n3654\n\n5014\n1838\n1302\n4994\n1105\n4055\n3542\n6260\n1219\n1737\n1191\n6069\n5293\n4765\n\n18556\n13230\n15065\n18284\n\n48135\n\n9070\n7739\n6829\n7932\n8713\n4312\n8262\n4925\n2769\n\n13528\n4077\n21766\n\n6782\n20020\n15396\n\n15610\n4166\n5153\n14554\n6008\n\n5875\n4131\n2371\n1381\n1793\n5487\n5816\n6051\n5595\n5412\n4800\n3069\n2332\n4360\n2380\n\n3310\n5210\n13776\n3665\n2335\n6094\n\n2728\n4544\n6050\n8616\n8297\n5501\n1544\n5770\n3807\n5388\n\n10758\n7573\n7170\n4945\n8868\n8074\n8055\n\n6053\n6804\n6870\n9779\n9992\n7935\n\n25222\n8776\n10549\n\n2785\n37443\n\n7055\n7876\n6587\n6322\n6447\n\n2403\n3529\n1514\n2202\n2622\n4484\n3216\n7798\n1135\n2120\n1624\n\n12169\n8212\n13127\n11115\n7072\n3080\n\n5017\n6458\n5226\n1287\n6413\n7938\n4211\n6724\n4862\n1242\n7481\n\n47712\n\n8432\n3562\n5349\n2961\n1171\n5888\n10533\n6771\n\n31566\n13234\n\n52095\n\n11601\n2394\n1084\n4909\n5592\n5718\n3912\n\n10963\n12196\n15697\n3599\n8635\n\n22503\n4376\n\n9918\n8408\n1882\n6546\n7045\n3395\n6931\n9059\n\n4664\n3815\n3110\n1676\n5396\n\n9459\n2435\n9466\n8929\n2587\n8181\n5385\n1703\n7383\n\n1271\n6006\n4250\n2558\n4734\n1817\n4695\n5683\n6119\n5150\n1060\n3814\n6012\n3977\n\n2690\n1343\n3990\n2648\n5829\n5590\n5609\n1718\n2148\n5400\n1130\n4659\n3463\n\n3287\n2098\n5050\n6246\n6901\n2001\n2289\n1892\n7836\n1358\n5224\n\n7024\n3331\n2742\n6134\n4850\n6971\n4798\n6270\n3375\n3678\n2791\n\n2968\n7186\n7213\n1818\n3735\n6366\n1138\n5386\n2479\n3978\n6926\n\n10692\n9805\n18086\n13749\n\n9566\n1552\n7121\n2410\n8839\n1581\n7143\n5682\n4454\n\n2580\n5217\n5391\n3347\n3384\n5377\n2047\n2465\n4622\n5743\n3015\n1305\n1960\n\n15710\n1936\n\n2012\n1530\n4286\n3519\n4311\n5820\n2446\n5807\n4890\n1242\n4685\n\n28724\n\n1094\n1190\n2500\n3617\n1286\n2430\n8633\n6728\n7392\n6037\n\n7997\n8802\n4988\n6780\n5537\n7377\n1238\n5501\n\n3131\n8597\n7966\n6157\n4817\n2327\n4515\n7983\n3165\n6785\n\n6203\n2841\n5423\n2632\n7410\n4532\n2012\n8719\n7507\n1449\n\n37282\n\n3012\n2285\n4191\n2242\n2014\n3836\n1261\n1951\n4484\n5194\n1328\n1295\n5970\n5030\n3604\n\n6092\n2930\n1517\n1064\n6648\n6112\n1392\n6746\n4490\n1785\n3352\n2248\n4975\n\n1774\n2925\n5667\n4315\n2228\n4088\n2936\n6062\n4679\n5220\n3173\n2961\n3431\n1907\n3213\n\n11417\n6055\n10378\n11585\n11495\n3286\n\n4242\n2198\n1453\n2454\n2088\n1068\n3263\n2082\n4262\n3684\n3614\n1624\n1772\n3144\n2264\n\n6044\n5100\n1281\n4829\n5966\n5878\n\n1648\n3848\n2167\n1269\n2907\n5569\n2338\n3106\n3710\n2819\n3167\n6767\n1247\n\n11542\n5319\n7390\n10247\n5825\n3701\n5173\n\n24038\n13030\n12703\n\n21677\n13326\n\n7060\n1796\n1544\n5340\n2291\n3485\n3469\n4839\n1484\n\n5894\n1537\n11507\n6994\n8819\n9523\n1286\n\n2034\n6244\n4109\n1153\n3516\n1981\n5427\n6918\n5572\n6415\n6618\n\n7353\n11049\n9629\n9086\n6160\n10827\n\n4833\n4240\n5281\n2985\n5001\n8689\n5876\n7358\n3057\n1477\n\n64086\n\n5607\n\n3536\n10591\n7185\n8614\n1048\n4488\n8893\n\n20808\n3764\n4172\n\n11061\n3485\n\n2184\n4986\n6147\n1483\n4170\n7920\n3623\n4908\n2433\n8087\n5981\n\n4145\n5088\n3079\n2696\n1069\n6533\n6205\n4633\n3393\n1875\n6734\n4047\n4783\n\n16756\n30731\n\n27320\n12665\n\n2342\n1130\n2482\n1540\n7263\n6251\n3244\n7960\n7255\n5031\n5540\n\n6945\n10279\n5400\n3868\n4448\n1159\n7190\n1858\n\n7499\n7600\n3816\n6417\n11374\n\n53260\n\n10436\n7554\n7263\n7794\n2222\n10687\n3170\n\n9157\n3146\n1723\n6066\n9016\n7758\n2885\n4938\n\n4079\n1302\n5794\n4695\n3859\n2063\n1108\n2669\n4541\n2125\n3846\n6019\n5780\n5048\n\n11772\n3333\n8185\n3786\n7493\n10737\n2954\n\n5412\n4386\n2105\n4871\n1621\n2588\n6143\n1314\n4979\n3222\n1918\n4300\n\n3598\n3198\n4102\n8638\n3899\n8204\n2209\n\n3851\n5188\n5494\n4102\n2427\n2568\n5264\n3951\n5236\n4779\n2152\n2163\n4852\n1866\n\n4900\n4865\n1946\n1585\n2073\n6066\n3482\n1118\n1250\n2125\n6795\n4522\n3602\n\n1352\n6115\n7150\n6257\n7255\n3150\n3372\n4024\n3744\n1436\n4558\n5741\n\n5330\n7428\n6493\n4217\n9133\n2001\n2117\n8205\n4679\n\n1039\n5446\n1385\n6914\n2516\n1703\n3439\n5279\n5214\n3995\n2455\n2006\n4653\n\n11735\n6890\n18785\n11174\n\n6361\n4088\n8048\n2118\n6994\n3985\n7492\n2137\n7852\n7743\n5795\n\n1399\n5322\n7254\n\n2887\n3842\n2467\n7211\n5307\n6887\n3368\n4678\n1254\n1202\n\n4740\n5746\n1742\n2749\n6503\n2949\n1161\n5671\n6057\n5157\n1619\n5947\n6338\n4537\n\n1448\n1427\n15871\n4776\n10021\n\n7272\n5789\n6013\n3511\n2392\n8082\n9583\n6188\n\n42302\n\n1943\n3178\n7626\n7150\n2064\n1486\n7344\n1988\n6079\n\n7075\n1400\n4314\n5688\n4630\n6478\n1247\n5885\n6741\n6403\n3825\n\n5186\n2077\n3021\n4983\n4907\n3529\n3227\n4448\n1144\n\n4816\n1427\n8101\n6482\n8208\n9606\n5595\n5328\n7792\n\n6090\n2627\n1141\n2740\n4856\n5727\n2102\n2985\n5365\n1041\n1036\n1636\n2345\n2379\n4654\n\n7437\n9144\n4579\n1225\n7821\n6909\n1844\n6643\n5680\n\n9820\n20439\n\n10177\n1834\n5160\n6476\n8352\n6981\n6495\n4232\n\n19764\n2091\n13751\n\n9408\n21846\n23438\n\n36692\n\n2312\n23451\n21444\n\n8577\n16088\n6638\n3195\n\n8178\n2765\n5564\n3191\n4604\n1430\n4034\n6644\n4469\n3445\n\n17210\n34046\n\n8370\n5176\n1309\n6288\n7944\n1098\n7113\n\n1468\n3241\n4149\n7521\n5759\n4483\n2993\n6307\n7456\n2687\n1646\n\n5728\n1132\n6223\n4918\n5095\n4815\n5589\n4549\n1827\n2757\n2134\n3079\n4709\n\n3905\n4016\n2848\n1499\n2841\n4670\n4478\n3694\n2334\n3019\n3761\n3986\n5486\n\n1096\n3760\n4569\n4388\n2661\n6027\n7493\n5582\n6502\n2966\n\n7501\n1109\n8955\n3136\n6744\n6849\n1916\n3971\n3396\n\n1865\n1631\n1981\n1333\n1431\n5516\n7613\n1926\n3224\n4058\n\n9732\n9318\n1299\n7300\n9826\n6425\n7878\n10546\n\n10269\n17150\n1771\n10243\n\n17664\n19265\n1710\n18754\n\n3695\n4063\n1571\n6002\n3421\n2712\n5175\n3987\n2780\n5452\n2383\n4207\n6162\n5926\n\n5918\n2169\n5996\n3842\n5338\n1061\n1221\n3034\n1357\n1447\n2178\n5486\n2861\n4633\n1822\n\n9053\n9367\n2796\n13702\n10292\n2461\n\n20214\n25924\n5012\n\n6229\n3670\n9939\n6177\n5980\n9907\n9698\n\n3846\n6923\n9058\n9689\n7405\n3684\n9010\n5705\n2877\n\n13046\n8583\n2706\n1403\n4918\n10851\n\n1349\n11562\n14857\n2260\n8315\n\n1375\n3094\n3849\n8763\n7189\n2550\n5346\n1905\n3498\n1416\n\n34144\n11430\n\n11590\n4032\n7238\n2641\n4778\n4260\n6007\n\n1226\n1934\n6034\n3159\n2234\n3509\n4759\n5837\n1789\n4726\n1075\n5543\n2774\n3144\n\n8556\n1675\n12135\n10831\n10837\n\n3087\n1977\n3832\n6304\n4023\n4219\n1594\n4685\n2650\n1652\n3029\n3913\n3970\n2311\n\n1088\n15228\n3891\n9920\n7675\n\n4758\n2544\n3198\n7038\n4561\n5105\n7469\n6070\n4917\n1288\n2099\n2257\n\n10202\n1478\n1355\n10235\n9177\n7656\n10315\n5359\n\n1336\n4004\n2306\n1358\n2831\n2861\n6211\n1492\n5542\n6370\n2111\n4289\n5037\n3784\n\n3083\n3535\n2551\n4519\n2305\n5878\n2102\n1477\n5529\n2048\n5068\n5816\n2887\n2567\n3309\n\n11079\n4417\n19925\n\n3159\n14561\n24535\n\n4712\n2595\n2996\n2035\n3697\n2792\n1377\n1002\n4955\n1728\n5072\n5754\n2671\n1267\n5035\n\n7438\n2343\n6421\n3441\n6451\n3448\n3853\n7268\n5461\n4265\n5232\n\n4224\n1229\n2945\n2152\n2899\n1055\n4692\n2627\n5771\n5807\n4868\n2069\n\n7271\n1691\n1945\n1814\n8487\n7836\n11053\n\n4542\n6055\n9464\n1919\n6973\n1526\n3408\n5265\n7587\n\n26489\n25984\n\n13254\n34865\n\n3338\n2313\n1854\n1232\n4036\n2156\n3203\n3959\n4643\n4045\n2097\n5726\n3509\n3801\n\n1530\n5956\n5236\n6895\n2136\n8084\n1550\n6776\n1042\n5389\n4815\n\n1479\n2337\n5443\n5880\n8131\n9774\n4225\n5564\n\n9111\n3248\n5761\n8147\n3285\n11468")