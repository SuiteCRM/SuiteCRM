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

class XPSSpeechModelSpecSubModelSpec extends \Google\Model
{
  /**
   * @var string
   */
  public $biasingModelType;
  /**
   * @var string
   */
  public $clientId;
  /**
   * @var string
   */
  public $contextId;
  /**
   * @var bool
   */
  public $isEnhancedModel;

  /**
   * @param string
   */
  public function setBiasingModelType($biasingModelType)
  {
    $this->biasingModelType = $biasingModelType;
  }
  /**
   * @return string
   */
  public function getBiasingModelType()
  {
    return $this->biasingModelType;
  }
  /**
   * @param string
   */
  public function setClientId($clientId)
  {
    $this->clientId = $clientId;
  }
  /**
   * @return string
   */
  public function getClientId()
  {
    return $this->clientId;
  }
  /**
   * @param string
   */
  public function setContextId($contextId)
  {
    $this->contextId = $contextId;
  }
  /**
   * @return string
   */
  public function getContextId()
  {
    return $this->contextId;
  }
  /**
   * @param bool
   */
  public function setIsEnhancedModel($isEnhancedModel)
  {
    $this->isEnhancedModel = $isEnhancedModel;
  }
  /**
   * @return bool
   */
  public function getIsEnhancedModel()
  {
    return $this->isEnhancedModel;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSSpeechModelSpecSubModelSpec::class, 'Google_Service_CloudNaturalLanguage_XPSSpeechModelSpecSubModelSpec');
