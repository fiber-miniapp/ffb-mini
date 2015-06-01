FFB-MINI
========

* version: 1.0.1 (based on FFB 8.1)
* date: 2015/06/02
* contact: miniapp@riken.jp


FrontFlow/blueについて
----------------------

本ミニアプリは，東京大学生産技術研究所で開発が進められている有限要素法による
熱流体解析プログラム FrontFlow/blue(以下，FFB) version 8.1をベースにしています．
FFBは，MPIによる領域分割並列計算に対応しています．
また，京コンピュータ用にチューニングされたコードを含み，京コンピュータおよび
富士通FX10上での，自動並列によるスレッド並列計算に対応しています．
FFBの詳細および入手方法については，
[Thermo-Fluid Analysis Solvers for Large-Scale Assembly](http://www.ciss.iis.u-tokyo.ac.jp/riss/english/project/fluid/)および
[ソフトウェア紹介](http://www.ciss.iis.u-tokyo.ac.jp/software/)を参照ください．

オリジナルFFB連絡先: 山出吉伸<yyamade@iis.u-tokyo.ac.jp>


インストール
------------

本プログラムのビルド・実行には，Fortran90およびCコンパイラ，MPIライブラリ，GNU Makeが必要である．
富士通FX10/京コンピュータでの自動並列機能を使用するには[METISライブラリ](http://glaros.dtc.umn.edu/gkhome/views/metis)が必要である．
また，実行時メッシュ細分化機能を利用するには[REVOCAP_Refinerライブラリ](http://www.ciss.iis.u-tokyo.ac.jp/dl/index.php?pScdownload_6)が必要である．

1. 本パッケージを入手し，ファイルを展開

2. srcディレクトリに移動し、"make_setting"ファイルの内容を実行環境に合わせて
   編集する．同ディレクトリには，以下の設定例ファイルがある:
    * make_setting.intel : Intelコンパイラ
    * make_setting.gcc   : GCCコンパイラ
    * make_setting.k     : 富士通コンパイラ(京)
    * make_setting.fx10  : 富士通コンパイラ(東大FX10)

3. srcディレクトリで"make"を実行．
   正常にmakeが完了すると，binディレクトリ下に実行プログラムles3x.mpおよび
   ラッパプログラムffb_miniが作成される．


テスト
------

インタラクティブにMPIジョブが実行できる環境用に，簡単なテストスクリプトをtest
ディレクトリに用意してある．テストを実行するには， testディレクトリまたはsrc
ディレクトリで"make test"を実行する(京コンピュータでは"make test_k"，東大FX10では
"make test_fx10")．
このテストスクリプトは，8MPIプロセスでの並列計算を行い，見本計算結果との間で流速
データの比較を行い，計算結果の妥当性を確認する．


プログラム計算内容
------------------

本プログラムは，FFBパッケージに含まれる流体計算プログラム"les3x.mpi"をミニアプリ化
している．以下にles3x.mpiの基本的なループ構造を示す．


    時間ステップループ {

        # Crank-Nicolson法による速度場(U,V,W)の計算
        U計算ループ {
            # BiCGSTAB計算
        }
        V計算ループ {
            # BiCGSTAB計算
        }
        W計算ループ {
            # BiCGSTAB計算
        }

        # 圧力Poisson方程式の求解
        RCM外側ループ {
            RCM内側ループ {
                # BiCGSTAB計算
            }
        }

        # 速度場補正計算

    }


速度場の計算では，各方向成分ごとに，BiCGSTABにより速度場を求めている．
圧力Poisson方程式では，ディフォルトの設定では，Residual Cutting Method(RCM,
残差切除法)を用いて圧力場を求めている．les3x.mpiでは，RCMの内側ループとして，
反復回数を10に固定したBiCGSTABを使用している．

ラッパプログラムffb_miniは，les3x.mpiに必要な入力データを生成した後にles3x.mpiを
内部実行する．計算対象は矩形領域で，+Y面が速度(1,0,0)を持つ移動壁境界，その他の
面を固定壁境界(3次元キャビティ流)として計算している．

なお，浮動小数点演算は全て単精度で行っている．


プログラム実行方法
------------------

### les3x.mpi

プログラムles3x.mpiの実行方法については，オリジナルFFBのマニュアル
(doc/Manual_FrontFlow_blue.8.1_ja.pdf)を参照のこと．

### ffb_mini

ffb_miniでは，実行時に必要なパラメータは全てコマンドラインで指定する．
実行方法は以下のとおりである．

    > ffb_mini Npx Npy Npz N [key1=val1 key2=val2 ...]

`Npx`, `Npy`, `Npz`, `N`は必須のパラメータ引数で，単位立方体領域に対して，
メッシュ数および領域分割方法を弱スケーリング的に指定する．すなわち，
`Npx`x`Npy`x`Npz`のMPIプロセスが，おのおの`N`^3個の正六面体要素からなる領域を
担当する．


`key1=val1`, `key2=val2`, ..は省略可能な引数で，以下のオプションが指定できる．

    ntime=int   計算ステップ数 (ディフォルト 100)
    isolp=int　 圧力方程式解法 1(BiCGSTAB) or 2(RCM) (ディフォルト 2)
    dt=float    時間刻み幅 (ディフォルト 0.005)
    nmaxp=int   圧力計算最大反復数(RCM内部反復の合計実行回数)
                (ディフォルト50)
    nmaxt=int   速度場計算最大反復数 (ディフォルト 5)
    epsp=float  圧力反復収束判定値
                0.0を指定すると，収束状況によらずnmaxpで指定した回数反復する
                (ディフォルト 0.0)
    epst=float  速度場計算反復収束判定値  
                0.0を指定すると，収束状況によらずnmaxtで指定した回数反復する
                (ディフォルト 0.0)
    color_partsize=int
                カラーリング領域サイズ(0以下を指定した場合カラーリングなし)
                (ディフォルト 0)
    reorder_ndiv=int
                節点リオーダブロックサイズ(0以下を指定した場合リオーダなし)
                (ディフォルト 0)
    unroll=str  行列ベクトル積計算(サブルーチンCALAX3)フラグ
                on(フルアンローリングあり) or off(なし) (ディフォルト off)


最後の3つのオプションについては，京コンピュータおよび富士通FX10での実行では，
以下の指定を推奨する．

    color_partsize=2000 reorder_ndiv=10 unroll=on

### 出力ファイル

ラッパプログラムffb_miniは以下のファイルを出力する．

* MESH.PXXXX      ... メッシュデータ
* BOUN.PXXXX      ... 境界条件データ
* PARMLES3X.PXXXX ... 実行時設定パラメータ
* FLOW.PXXXX      ... 計算結果流速場データ
* les3x.log.PXXXX ... 実行時ログ出力

ここで，拡張子PXXXXの"XXXX"は，1から始まる実行MPIプロセス数までの整数値である．
ただし，実行時ログ出力については，les3x.log.P0001の代わりに，ランク0のログは
標準出力に出力される．また，MESH.PXXXX,BOUN.PXXXX,PARMLES3X.PXXXXは，ffb_mini
内部で実行されるles3x.mpiに対しては入力ファイルとなっている．


エクサスケールでの想定計算規模
------------------------------

1兆(10^12)要素に対し10秒/ステップで計算することを目指す．
(4万ステップの計算を100時間程度で終わらせたい)


オリジナルFFB8.1からの変更点
----------------------------

* les3x.mpi以外のプログラムを削除

* 機能を限定することにより，コードを削減

    - 六面体要素，圧力要素定義による非定常流体解析計算に限定
    - 乱流モデル，キャビテーションモデル，VOF，オーバセット・マルチーフレーム，
      ALE，ミッドノード，ファンモデル・ポーラスモデル機能を削除
    - サンプリング機能，ヒストリ出力機能を削除

* METISおよびREVOCAP_Refinerライブラリの分離

* ベンチマーク用ラッパプログラムを追加

* 計算結果検証用スクリプトを追加

* その他のコード整備


詳細情報
--------

### ホットスポット

圧力Poisson方程式を解くループ内でラプラシアンを計算するサブルーチンCALLAP内から
呼ばれる，勾配計算GRAD3Xおよび発散計算FLD3X2ルーチン，また，速度場計算BiCGSTAB内
から呼ばれる行列ベクトル積を計算するCALAX3ルーチンがホットスポットとなる．
各ホットスポットルーチンの1時間ステップあたりでの呼び出し回数は，それぞれ以下の
とおりとなる．

  * GRAD3X: 2.4 * NMAXP + 1
  * FLD3X2: 2.4 * NMAXP + 1
  * CALAX3: 6 * NMAXT + 3

ここで，NMAXPは圧力反復ループ数(RCM内部反復の合計実行回数)，NMAXTは速度成分計算
BiCGSTAB計算での反復回数である．

### MPI プロセス間通信

時間ステップループ内では，以下の2つの通信がある．

  * 領域間共有節点上データの重ね合わせ通信(「袖通信」に相当)，サブルーチンDDCOMX
  * 内積計算時のAllreduce通信，サブルーチンDDCOM2

RCMによる圧力Poisson方程式解法ルーチン(RCMELM)内での通信ルーチン呼び出し回数は

  * DDCOMX: 2.4 * NMAXP + 1
  * DDCOM2: 5.5 * NMAXP + 2

BiCGSTABによる速度場計算ルーチン(VEL3D1)内での内での通信ルーチン呼び出し回数は

  * DDCOMX: 6 * NMAXT + 3
  * DDCOM2: 15 * NMAXT + 6  

となる．


参考文献
--------

* Minami et al., "Performance Tuning and Evaluation of Sparse matrix-vector
  multiplication on the K computer",
  IPSJ High Performance Computing Symposium, 2012. pp.23-31, 2012. (in Japanese)
* Kumahata et al., "Kernel performance improvement for the FEM-based fluid
  analysis code on the K computer",
  Procedia Computer Science Vol.18, pp.2496-2499, 2013.
* Kumahata et al.,"Performance Tuning for Gradient Kernel of the
  FrontFlow/blue on the K computer",
  IPSJ Transactions on Advanced Computing Systems Vol.6, No.3 pp.31-42, 2013.
  (in Japanese)


Version History
---------------

* 1.0.0 (May 21, 2015)
    - Inital release

* 1.0.1 (June 2, 2015)
    - Fix a bug in makemesh

