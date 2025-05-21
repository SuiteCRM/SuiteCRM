<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\Aiplatform;

class CloudAiLargeModelsVisionImage extends \Google\Model
{
  /**
   * @var string
   */
  public $encoding;
  /**
   * @var int
   */
  public $generationSeed;
  /**
   * @var string
   */
  public $image;
  protected $imageRaiScoresType = CloudAiLargeModelsVisionImageRAIScores::class;
  protected $imageRaiScoresDataType = '';
  protected $imageSizeType = CloudAiLargeModelsVisionImageImageSize::class;
  protected $imageSizeDataType = '';
  protected $raiInfoType = CloudAiLargeModelsVisionRaiInfo::class;
  protected $raiInfoDataType = '';
  protected $semanticFilterResponseType = CloudAiLargeModelsVisionSemanticFilterResponse::class;
  protected $semanticFilterResponseDataType = '';
  /**
   * @var string
   */
  public $text;
  /**
   * @var string
   */
  public $uri;

  /**
   * @param string
   */
  public function setEncoding($encoding)
  {
    $this->encoding = $encoding;
  }
  /**
   * @return string
   */
  public function getEncoding()
  {
    return $this->encoding;
  }
  /**
   * @param int
   */
  public function setGenerationSeed($generationSeed)
  {
    $this->generationSeed = $generationSeed;
  }
  /**
   * @return int
   */
  public function getGenerationSeed()
  {
    return $this->generationSeed;
  }
  /**
   * @param string
   */
  public function setImage($image)
  {
    $this->image = $image;
  }
  /**
   * @return string
   */
  public function getImage()
  {
    return $this->image;
  }
  /**
   * @param CloudAiLargeModelsVisionImageRAIScores
   */
  public function setImageRaiScores(CloudAiLargeModelsVisionImageRAIScores $imageRaiScores)
  {
    $this->imageRaiScores = $imageRaiScores;
  }
  /**
   * @return CloudAiLargeModelsVisionImageRAIScores
   */
  public function getImageRaiScores()
  {
    return $this->imageRaiScores;
  }
  /**
   * @param CloudAiLargeModelsVisionImageImageSize
   */
  public function setImageSize(CloudAiLargeModelsVisionImageImageSize $imageSize)
  {
    $this->imageSize = $imageSize;
  }
  /**
   * @return CloudAiLargeModelsVisionImageImageSize
   */
  public function getImageSize()
  {
    return $this->imageSize;
  }
  /**
   * @param CloudAiLargeModelsVisionRaiInfo
   */
  public function setRaiInfo(CloudAiLargeModelsVisionRaiInfo $raiInfo)
  {
    $this->raiInfo = $raiInfo;
  }
  /**
   * @return CloudAiLargeModelsVisionRaiInfo
   */
  public function getRaiInfo()
  {
    return $this->raiInfo;
  }
  /**
   * @param CloudAiLargeModelsVisionSemanticFilterResponse
   */
  public function setSemanticFilterResponse(CloudAiLargeModelsVisionSemanticFilterResponse $semanticFilterResponse)
  {
    $this->semanticFilterResponse = $semanticFilterResponse;
  }
  /**
   * @return CloudAiLargeModelsVisionSemanticFilterResponse
   */
  public function getSemanticFilterResponse()
  {
    return $this->semanticFilterResponse;
  }
  /**
   * @param string
   */
  public function setText($text)
  {
    $this->text = $text;
  }
  /**
   * @return string
   */
  public function getText()
  {
    return $this->text;
  }
  /**
   * @param string
   */
  public function setUri($uri)
  {
    $this->uri = $uri;
  }
  /**
   * @return string
   */
  public function getUri()
  {
    return $this->uri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudAiLargeModelsVisionImage::class, 'Google_Service_Aiplatform_CloudAiLargeModelsVisionImage');
