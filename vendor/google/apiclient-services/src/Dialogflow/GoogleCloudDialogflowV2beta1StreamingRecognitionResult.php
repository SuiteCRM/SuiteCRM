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

namespace Google\Service\Dialogflow;

class GoogleCloudDialogflowV2beta1StreamingRecognitionResult extends \Google\Collection
{
  protected $collection_key = 'speechWordInfo';
  /**
   * @var float
   */
  public $confidence;
  protected $dtmfDigitsType = GoogleCloudDialogflowV2beta1TelephonyDtmfEvents::class;
  protected $dtmfDigitsDataType = '';
  /**
   * @var bool
   */
  public $isFinal;
  /**
   * @var string
   */
  public $languageCode;
  /**
   * @var string
   */
  public $messageType;
  /**
   * @var string
   */
  public $speechEndOffset;
  protected $speechWordInfoType = GoogleCloudDialogflowV2beta1SpeechWordInfo::class;
  protected $speechWordInfoDataType = 'array';
  /**
   * @var float
   */
  public $stability;
  /**
   * @var string
   */
  public $transcript;

  /**
   * @param float
   */
  public function setConfidence($confidence)
  {
    $this->confidence = $confidence;
  }
  /**
   * @return float
   */
  public function getConfidence()
  {
    return $this->confidence;
  }
  /**
   * @param GoogleCloudDialogflowV2beta1TelephonyDtmfEvents
   */
  public function setDtmfDigits(GoogleCloudDialogflowV2beta1TelephonyDtmfEvents $dtmfDigits)
  {
    $this->dtmfDigits = $dtmfDigits;
  }
  /**
   * @return GoogleCloudDialogflowV2beta1TelephonyDtmfEvents
   */
  public function getDtmfDigits()
  {
    return $this->dtmfDigits;
  }
  /**
   * @param bool
   */
  public function setIsFinal($isFinal)
  {
    $this->isFinal = $isFinal;
  }
  /**
   * @return bool
   */
  public function getIsFinal()
  {
    return $this->isFinal;
  }
  /**
   * @param string
   */
  public function setLanguageCode($languageCode)
  {
    $this->languageCode = $languageCode;
  }
  /**
   * @return string
   */
  public function getLanguageCode()
  {
    return $this->languageCode;
  }
  /**
   * @param string
   */
  public function setMessageType($messageType)
  {
    $this->messageType = $messageType;
  }
  /**
   * @return string
   */
  public function getMessageType()
  {
    return $this->messageType;
  }
  /**
   * @param string
   */
  public function setSpeechEndOffset($speechEndOffset)
  {
    $this->speechEndOffset = $speechEndOffset;
  }
  /**
   * @return string
   */
  public function getSpeechEndOffset()
  {
    return $this->speechEndOffset;
  }
  /**
   * @param GoogleCloudDialogflowV2beta1SpeechWordInfo[]
   */
  public function setSpeechWordInfo($speechWordInfo)
  {
    $this->speechWordInfo = $speechWordInfo;
  }
  /**
   * @return GoogleCloudDialogflowV2beta1SpeechWordInfo[]
   */
  public function getSpeechWordInfo()
  {
    return $this->speechWordInfo;
  }
  /**
   * @param float
   */
  public function setStability($stability)
  {
    $this->stability = $stability;
  }
  /**
   * @return float
   */
  public function getStability()
  {
    return $this->stability;
  }
  /**
   * @param string
   */
  public function setTranscript($transcript)
  {
    $this->transcript = $transcript;
  }
  /**
   * @return string
   */
  public function getTranscript()
  {
    return $this->transcript;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDialogflowV2beta1StreamingRecognitionResult::class, 'Google_Service_Dialogflow_GoogleCloudDialogflowV2beta1StreamingRecognitionResult');
