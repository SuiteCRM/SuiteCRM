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

class XPSEvaluationMetrics extends \Google\Model
{
  /**
   * @var string
   */
  public $annotationSpecIdToken;
  /**
   * @var int
   */
  public $category;
  /**
   * @var int
   */
  public $evaluatedExampleCount;
  protected $imageClassificationEvalMetricsType = XPSClassificationEvaluationMetrics::class;
  protected $imageClassificationEvalMetricsDataType = '';
  protected $imageObjectDetectionEvalMetricsType = XPSImageObjectDetectionEvaluationMetrics::class;
  protected $imageObjectDetectionEvalMetricsDataType = '';
  protected $imageSegmentationEvalMetricsType = XPSImageSegmentationEvaluationMetrics::class;
  protected $imageSegmentationEvalMetricsDataType = '';
  /**
   * @var string
   */
  public $label;
  protected $regressionEvalMetricsType = XPSRegressionEvaluationMetrics::class;
  protected $regressionEvalMetricsDataType = '';
  protected $tablesClassificationEvalMetricsType = XPSClassificationEvaluationMetrics::class;
  protected $tablesClassificationEvalMetricsDataType = '';
  protected $tablesEvalMetricsType = XPSTablesEvaluationMetrics::class;
  protected $tablesEvalMetricsDataType = '';
  protected $textClassificationEvalMetricsType = XPSClassificationEvaluationMetrics::class;
  protected $textClassificationEvalMetricsDataType = '';
  protected $textExtractionEvalMetricsType = XPSTextExtractionEvaluationMetrics::class;
  protected $textExtractionEvalMetricsDataType = '';
  protected $textSentimentEvalMetricsType = XPSTextSentimentEvaluationMetrics::class;
  protected $textSentimentEvalMetricsDataType = '';
  protected $translationEvalMetricsType = XPSTranslationEvaluationMetrics::class;
  protected $translationEvalMetricsDataType = '';
  protected $videoActionRecognitionEvalMetricsType = XPSVideoActionRecognitionEvaluationMetrics::class;
  protected $videoActionRecognitionEvalMetricsDataType = '';
  protected $videoClassificationEvalMetricsType = XPSClassificationEvaluationMetrics::class;
  protected $videoClassificationEvalMetricsDataType = '';
  protected $videoObjectTrackingEvalMetricsType = XPSVideoObjectTrackingEvaluationMetrics::class;
  protected $videoObjectTrackingEvalMetricsDataType = '';

  /**
   * @param string
   */
  public function setAnnotationSpecIdToken($annotationSpecIdToken)
  {
    $this->annotationSpecIdToken = $annotationSpecIdToken;
  }
  /**
   * @return string
   */
  public function getAnnotationSpecIdToken()
  {
    return $this->annotationSpecIdToken;
  }
  /**
   * @param int
   */
  public function setCategory($category)
  {
    $this->category = $category;
  }
  /**
   * @return int
   */
  public function getCategory()
  {
    return $this->category;
  }
  /**
   * @param int
   */
  public function setEvaluatedExampleCount($evaluatedExampleCount)
  {
    $this->evaluatedExampleCount = $evaluatedExampleCount;
  }
  /**
   * @return int
   */
  public function getEvaluatedExampleCount()
  {
    return $this->evaluatedExampleCount;
  }
  /**
   * @param XPSClassificationEvaluationMetrics
   */
  public function setImageClassificationEvalMetrics(XPSClassificationEvaluationMetrics $imageClassificationEvalMetrics)
  {
    $this->imageClassificationEvalMetrics = $imageClassificationEvalMetrics;
  }
  /**
   * @return XPSClassificationEvaluationMetrics
   */
  public function getImageClassificationEvalMetrics()
  {
    return $this->imageClassificationEvalMetrics;
  }
  /**
   * @param XPSImageObjectDetectionEvaluationMetrics
   */
  public function setImageObjectDetectionEvalMetrics(XPSImageObjectDetectionEvaluationMetrics $imageObjectDetectionEvalMetrics)
  {
    $this->imageObjectDetectionEvalMetrics = $imageObjectDetectionEvalMetrics;
  }
  /**
   * @return XPSImageObjectDetectionEvaluationMetrics
   */
  public function getImageObjectDetectionEvalMetrics()
  {
    return $this->imageObjectDetectionEvalMetrics;
  }
  /**
   * @param XPSImageSegmentationEvaluationMetrics
   */
  public function setImageSegmentationEvalMetrics(XPSImageSegmentationEvaluationMetrics $imageSegmentationEvalMetrics)
  {
    $this->imageSegmentationEvalMetrics = $imageSegmentationEvalMetrics;
  }
  /**
   * @return XPSImageSegmentationEvaluationMetrics
   */
  public function getImageSegmentationEvalMetrics()
  {
    return $this->imageSegmentationEvalMetrics;
  }
  /**
   * @param string
   */
  public function setLabel($label)
  {
    $this->label = $label;
  }
  /**
   * @return string
   */
  public function getLabel()
  {
    return $this->label;
  }
  /**
   * @param XPSRegressionEvaluationMetrics
   */
  public function setRegressionEvalMetrics(XPSRegressionEvaluationMetrics $regressionEvalMetrics)
  {
    $this->regressionEvalMetrics = $regressionEvalMetrics;
  }
  /**
   * @return XPSRegressionEvaluationMetrics
   */
  public function getRegressionEvalMetrics()
  {
    return $this->regressionEvalMetrics;
  }
  /**
   * @param XPSClassificationEvaluationMetrics
   */
  public function setTablesClassificationEvalMetrics(XPSClassificationEvaluationMetrics $tablesClassificationEvalMetrics)
  {
    $this->tablesClassificationEvalMetrics = $tablesClassificationEvalMetrics;
  }
  /**
   * @return XPSClassificationEvaluationMetrics
   */
  public function getTablesClassificationEvalMetrics()
  {
    return $this->tablesClassificationEvalMetrics;
  }
  /**
   * @param XPSTablesEvaluationMetrics
   */
  public function setTablesEvalMetrics(XPSTablesEvaluationMetrics $tablesEvalMetrics)
  {
    $this->tablesEvalMetrics = $tablesEvalMetrics;
  }
  /**
   * @return XPSTablesEvaluationMetrics
   */
  public function getTablesEvalMetrics()
  {
    return $this->tablesEvalMetrics;
  }
  /**
   * @param XPSClassificationEvaluationMetrics
   */
  public function setTextClassificationEvalMetrics(XPSClassificationEvaluationMetrics $textClassificationEvalMetrics)
  {
    $this->textClassificationEvalMetrics = $textClassificationEvalMetrics;
  }
  /**
   * @return XPSClassificationEvaluationMetrics
   */
  public function getTextClassificationEvalMetrics()
  {
    return $this->textClassificationEvalMetrics;
  }
  /**
   * @param XPSTextExtractionEvaluationMetrics
   */
  public function setTextExtractionEvalMetrics(XPSTextExtractionEvaluationMetrics $textExtractionEvalMetrics)
  {
    $this->textExtractionEvalMetrics = $textExtractionEvalMetrics;
  }
  /**
   * @return XPSTextExtractionEvaluationMetrics
   */
  public function getTextExtractionEvalMetrics()
  {
    return $this->textExtractionEvalMetrics;
  }
  /**
   * @param XPSTextSentimentEvaluationMetrics
   */
  public function setTextSentimentEvalMetrics(XPSTextSentimentEvaluationMetrics $textSentimentEvalMetrics)
  {
    $this->textSentimentEvalMetrics = $textSentimentEvalMetrics;
  }
  /**
   * @return XPSTextSentimentEvaluationMetrics
   */
  public function getTextSentimentEvalMetrics()
  {
    return $this->textSentimentEvalMetrics;
  }
  /**
   * @param XPSTranslationEvaluationMetrics
   */
  public function setTranslationEvalMetrics(XPSTranslationEvaluationMetrics $translationEvalMetrics)
  {
    $this->translationEvalMetrics = $translationEvalMetrics;
  }
  /**
   * @return XPSTranslationEvaluationMetrics
   */
  public function getTranslationEvalMetrics()
  {
    return $this->translationEvalMetrics;
  }
  /**
   * @param XPSVideoActionRecognitionEvaluationMetrics
   */
  public function setVideoActionRecognitionEvalMetrics(XPSVideoActionRecognitionEvaluationMetrics $videoActionRecognitionEvalMetrics)
  {
    $this->videoActionRecognitionEvalMetrics = $videoActionRecognitionEvalMetrics;
  }
  /**
   * @return XPSVideoActionRecognitionEvaluationMetrics
   */
  public function getVideoActionRecognitionEvalMetrics()
  {
    return $this->videoActionRecognitionEvalMetrics;
  }
  /**
   * @param XPSClassificationEvaluationMetrics
   */
  public function setVideoClassificationEvalMetrics(XPSClassificationEvaluationMetrics $videoClassificationEvalMetrics)
  {
    $this->videoClassificationEvalMetrics = $videoClassificationEvalMetrics;
  }
  /**
   * @return XPSClassificationEvaluationMetrics
   */
  public function getVideoClassificationEvalMetrics()
  {
    return $this->videoClassificationEvalMetrics;
  }
  /**
   * @param XPSVideoObjectTrackingEvaluationMetrics
   */
  public function setVideoObjectTrackingEvalMetrics(XPSVideoObjectTrackingEvaluationMetrics $videoObjectTrackingEvalMetrics)
  {
    $this->videoObjectTrackingEvalMetrics = $videoObjectTrackingEvalMetrics;
  }
  /**
   * @return XPSVideoObjectTrackingEvaluationMetrics
   */
  public function getVideoObjectTrackingEvalMetrics()
  {
    return $this->videoObjectTrackingEvalMetrics;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSEvaluationMetrics::class, 'Google_Service_CloudNaturalLanguage_XPSEvaluationMetrics');
