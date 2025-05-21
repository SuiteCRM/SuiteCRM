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

namespace Google\Service\CloudNaturalLanguage;

class XPSTrainResponse extends \Google\Collection
{
  protected $collection_key = 'explanationConfigs';
  /**
   * @var string
   */
  public $deployedModelSizeBytes;
  protected $errorAnalysisConfigsType = XPSVisionErrorAnalysisConfig::class;
  protected $errorAnalysisConfigsDataType = 'array';
  protected $evaluatedExampleSetType = XPSExampleSet::class;
  protected $evaluatedExampleSetDataType = '';
  protected $evaluationMetricsSetType = XPSEvaluationMetricsSet::class;
  protected $evaluationMetricsSetDataType = '';
  protected $explanationConfigsType = XPSResponseExplanationSpec::class;
  protected $explanationConfigsDataType = 'array';
  protected $imageClassificationTrainRespType = XPSImageClassificationTrainResponse::class;
  protected $imageClassificationTrainRespDataType = '';
  protected $imageObjectDetectionTrainRespType = XPSImageObjectDetectionModelSpec::class;
  protected $imageObjectDetectionTrainRespDataType = '';
  protected $imageSegmentationTrainRespType = XPSImageSegmentationTrainResponse::class;
  protected $imageSegmentationTrainRespDataType = '';
  /**
   * @var string
   */
  public $modelToken;
  protected $speechTrainRespType = XPSSpeechModelSpec::class;
  protected $speechTrainRespDataType = '';
  protected $tablesTrainRespType = XPSTablesTrainResponse::class;
  protected $tablesTrainRespDataType = '';
  protected $textToSpeechTrainRespType = XPSTextToSpeechTrainResponse::class;
  protected $textToSpeechTrainRespDataType = '';
  protected $textTrainRespType = XPSTextTrainResponse::class;
  protected $textTrainRespDataType = '';
  protected $translationTrainRespType = XPSTranslationTrainResponse::class;
  protected $translationTrainRespDataType = '';
  protected $videoActionRecognitionTrainRespType = XPSVideoActionRecognitionTrainResponse::class;
  protected $videoActionRecognitionTrainRespDataType = '';
  protected $videoClassificationTrainRespType = XPSVideoClassificationTrainResponse::class;
  protected $videoClassificationTrainRespDataType = '';
  protected $videoObjectTrackingTrainRespType = XPSVideoObjectTrackingTrainResponse::class;
  protected $videoObjectTrackingTrainRespDataType = '';

  /**
   * @param string
   */
  public function setDeployedModelSizeBytes($deployedModelSizeBytes)
  {
    $this->deployedModelSizeBytes = $deployedModelSizeBytes;
  }
  /**
   * @return string
   */
  public function getDeployedModelSizeBytes()
  {
    return $this->deployedModelSizeBytes;
  }
  /**
   * @param XPSVisionErrorAnalysisConfig[]
   */
  public function setErrorAnalysisConfigs($errorAnalysisConfigs)
  {
    $this->errorAnalysisConfigs = $errorAnalysisConfigs;
  }
  /**
   * @return XPSVisionErrorAnalysisConfig[]
   */
  public function getErrorAnalysisConfigs()
  {
    return $this->errorAnalysisConfigs;
  }
  /**
   * @param XPSExampleSet
   */
  public function setEvaluatedExampleSet(XPSExampleSet $evaluatedExampleSet)
  {
    $this->evaluatedExampleSet = $evaluatedExampleSet;
  }
  /**
   * @return XPSExampleSet
   */
  public function getEvaluatedExampleSet()
  {
    return $this->evaluatedExampleSet;
  }
  /**
   * @param XPSEvaluationMetricsSet
   */
  public function setEvaluationMetricsSet(XPSEvaluationMetricsSet $evaluationMetricsSet)
  {
    $this->evaluationMetricsSet = $evaluationMetricsSet;
  }
  /**
   * @return XPSEvaluationMetricsSet
   */
  public function getEvaluationMetricsSet()
  {
    return $this->evaluationMetricsSet;
  }
  /**
   * @param XPSResponseExplanationSpec[]
   */
  public function setExplanationConfigs($explanationConfigs)
  {
    $this->explanationConfigs = $explanationConfigs;
  }
  /**
   * @return XPSResponseExplanationSpec[]
   */
  public function getExplanationConfigs()
  {
    return $this->explanationConfigs;
  }
  /**
   * @param XPSImageClassificationTrainResponse
   */
  public function setImageClassificationTrainResp(XPSImageClassificationTrainResponse $imageClassificationTrainResp)
  {
    $this->imageClassificationTrainResp = $imageClassificationTrainResp;
  }
  /**
   * @return XPSImageClassificationTrainResponse
   */
  public function getImageClassificationTrainResp()
  {
    return $this->imageClassificationTrainResp;
  }
  /**
   * @param XPSImageObjectDetectionModelSpec
   */
  public function setImageObjectDetectionTrainResp(XPSImageObjectDetectionModelSpec $imageObjectDetectionTrainResp)
  {
    $this->imageObjectDetectionTrainResp = $imageObjectDetectionTrainResp;
  }
  /**
   * @return XPSImageObjectDetectionModelSpec
   */
  public function getImageObjectDetectionTrainResp()
  {
    return $this->imageObjectDetectionTrainResp;
  }
  /**
   * @param XPSImageSegmentationTrainResponse
   */
  public function setImageSegmentationTrainResp(XPSImageSegmentationTrainResponse $imageSegmentationTrainResp)
  {
    $this->imageSegmentationTrainResp = $imageSegmentationTrainResp;
  }
  /**
   * @return XPSImageSegmentationTrainResponse
   */
  public function getImageSegmentationTrainResp()
  {
    return $this->imageSegmentationTrainResp;
  }
  /**
   * @param string
   */
  public function setModelToken($modelToken)
  {
    $this->modelToken = $modelToken;
  }
  /**
   * @return string
   */
  public function getModelToken()
  {
    return $this->modelToken;
  }
  /**
   * @param XPSSpeechModelSpec
   */
  public function setSpeechTrainResp(XPSSpeechModelSpec $speechTrainResp)
  {
    $this->speechTrainResp = $speechTrainResp;
  }
  /**
   * @return XPSSpeechModelSpec
   */
  public function getSpeechTrainResp()
  {
    return $this->speechTrainResp;
  }
  /**
   * @param XPSTablesTrainResponse
   */
  public function setTablesTrainResp(XPSTablesTrainResponse $tablesTrainResp)
  {
    $this->tablesTrainResp = $tablesTrainResp;
  }
  /**
   * @return XPSTablesTrainResponse
   */
  public function getTablesTrainResp()
  {
    return $this->tablesTrainResp;
  }
  /**
   * @param XPSTextToSpeechTrainResponse
   */
  public function setTextToSpeechTrainResp(XPSTextToSpeechTrainResponse $textToSpeechTrainResp)
  {
    $this->textToSpeechTrainResp = $textToSpeechTrainResp;
  }
  /**
   * @return XPSTextToSpeechTrainResponse
   */
  public function getTextToSpeechTrainResp()
  {
    return $this->textToSpeechTrainResp;
  }
  /**
   * @param XPSTextTrainResponse
   */
  public function setTextTrainResp(XPSTextTrainResponse $textTrainResp)
  {
    $this->textTrainResp = $textTrainResp;
  }
  /**
   * @return XPSTextTrainResponse
   */
  public function getTextTrainResp()
  {
    return $this->textTrainResp;
  }
  /**
   * @param XPSTranslationTrainResponse
   */
  public function setTranslationTrainResp(XPSTranslationTrainResponse $translationTrainResp)
  {
    $this->translationTrainResp = $translationTrainResp;
  }
  /**
   * @return XPSTranslationTrainResponse
   */
  public function getTranslationTrainResp()
  {
    return $this->translationTrainResp;
  }
  /**
   * @param XPSVideoActionRecognitionTrainResponse
   */
  public function setVideoActionRecognitionTrainResp(XPSVideoActionRecognitionTrainResponse $videoActionRecognitionTrainResp)
  {
    $this->videoActionRecognitionTrainResp = $videoActionRecognitionTrainResp;
  }
  /**
   * @return XPSVideoActionRecognitionTrainResponse
   */
  public function getVideoActionRecognitionTrainResp()
  {
    return $this->videoActionRecognitionTrainResp;
  }
  /**
   * @param XPSVideoClassificationTrainResponse
   */
  public function setVideoClassificationTrainResp(XPSVideoClassificationTrainResponse $videoClassificationTrainResp)
  {
    $this->videoClassificationTrainResp = $videoClassificationTrainResp;
  }
  /**
   * @return XPSVideoClassificationTrainResponse
   */
  public function getVideoClassificationTrainResp()
  {
    return $this->videoClassificationTrainResp;
  }
  /**
   * @param XPSVideoObjectTrackingTrainResponse
   */
  public function setVideoObjectTrackingTrainResp(XPSVideoObjectTrackingTrainResponse $videoObjectTrackingTrainResp)
  {
    $this->videoObjectTrackingTrainResp = $videoObjectTrackingTrainResp;
  }
  /**
   * @return XPSVideoObjectTrackingTrainResponse
   */
  public function getVideoObjectTrackingTrainResp()
  {
    return $this->videoObjectTrackingTrainResp;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSTrainResponse::class, 'Google_Service_CloudNaturalLanguage_XPSTrainResponse');
