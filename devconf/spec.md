Postludium Device Configuration 仕様書
---

## トップレベル要素
### `Include`

```
Include (StringLiteral)
```

他のpdcファイルを組み込む。プリプロセッサと違ってファイルは展開されず再帰してコンパイルされる。

### `Extern`

Rustソースコードから与えられる外部リソースを定義する。

```
[$Name: ] Extern ImageView (1D/2D/3D) (refname)
```

イメージビューオブジェクトを定義する。

```
[$Name: ] Extern SwapChainViews
```

スワップチェーンのビューを定義する。スワップチェーンのバックバッファの数に応じて対象リソースが生成されるようになる。

### `RenderPass`

```
[$Name: ] RenderPass
- Attachments:
-- [$Name: ] (format), (transition), (option,*)
- Subpasses:
-- [$Name: ] [RenderTo (attachments...) / From (attachments...)]*
- Dependencies:
-- (subpass) [[From/To]] (subpass): (transition) At (stage_bits) [, ByRegion]
```

RenderPassを定義する。Attachmentsのtransitionは遷移先を省略することができる(省略すると遷移元と同じものを指定したことになる)。

### `SimpleRenderPass`

```
[$Name: ] SimpleRenderPass
```

よく使われるRenderPassの構成を簡潔に記述できるようにする。

### `PresentedRenderPass`

```
[$Name: ] PresentedRenderPass
```

表示用によく使われるRenderPassの構成を簡潔に記述できるようにする。

### `Framebuffer`

```
[$Name: ] Framebuffer<(RenderPass)> (views...)
```

フレームバッファオブジェクトを定義する。

### `DescriptorSetLayout`

```
[$Name: ] DescriptorSetLayout
```

DescriptorSetのレイアウトを定義する。

### `PushConstantLayout`

```
[$Name: ] PushConstantLayout
```

PushConstantのレイアウトを定義する。

### `PipelineLayout`

```
[$Name: ] PipelineLayout
```

パイプラインレイアウトを定義する。

### `DescriptorSets`

```
[$Name: ] DescriptorSets
```

Descriptorの配列を定義する。

### `PipelineState`

```
[$Name: ] PipelineState for (RenderPass).(SubpassIndex) with (PipelineLayout)
```

パイプラインステートを定義する。

### 各種アセット特殊化

```
[$Name: ] VertexShader (AssetName)
[$Name: ] FragmentShader (AssetName)
[$Name: ] GeometryShader (AssetName)
[$Name: ] TessellationControlShader (AssetName)
[$Name: ] TessellationEvaluationShader (AssetName)
```

各種アセットを特殊化してパイプラインステート定義で使えるようにする。
