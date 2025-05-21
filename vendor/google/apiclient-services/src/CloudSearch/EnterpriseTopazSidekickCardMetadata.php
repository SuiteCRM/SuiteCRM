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

namespace Google\Service\CloudSearch;

class EnterpriseTopazSidekickCardMetadata extends \Google\Model
{
  /**
   * @var string
   */
  public $cardCategory;
  /**
   * @var string
   */
  public $cardId;
  /**
   * @var string
   */
  public $chronology;
  /**
   * @var string
   */
  public $debugInfo;
  protected $nlpMetadataType = EnterpriseTopazSidekickNlpMetadata::class;
  protected $nlpMetadataDataType = '';
  protected $rankingParamsType = EnterpriseTopazSidekickRankingParams::class;
  protected $rankingParamsDataType = '';
  /**
   * @var string
   */
  public $renderMode;

  /**
   * @param string
   */
  public function setCardCategory($cardCategory)
  {
    $this->cardCategory = $cardCategory;
  }
  /**
   * @return string
   */
  public function getCardCategory()
  {
    return $this->cardCategory;
  }
  /**
   * @param string
   */
  public function setCardId($cardId)
  {
    $this->cardId = $cardId;
  }
  /**
   * @return string
   */
  public function getCardId()
  {
    return $this->cardId;
  }
  /**
   * @param string
   */
  public function setChronology($chronology)
  {
    $this->chronology = $chronology;
  }
  /**
   * @return string
   */
  public function getChronology()
  {
    return $this->chronology;
  }
  /**
   * @param string
   */
  public function setDebugInfo($debugInfo)
  {
    $this->debugInfo = $debugInfo;
  }
  /**
   * @return string
   */
  public function getDebugInfo()
  {
    return $this->debugInfo;
  }
  /**
   * @param EnterpriseTopazSidekickNlpMetadata
   */
  public function setNlpMetadata(EnterpriseTopazSidekickNlpMetadata $nlpMetadata)
  {
    $this->nlpMetadata = $nlpMetadata;
  }
  /**
   * @return EnterpriseTopazSidekickNlpMetadata
   */
  public function getNlpMetadata()
  {
    return $this->nlpMetadata;
  }
  /**
   * @param EnterpriseTopazSidekickRankingParams
   */
  public function setRankingParams(EnterpriseTopazSidekickRankingParams $rankingParams)
  {
    $this->rankingParams = $rankingParams;
  }
  /**
   * @return EnterpriseTopazSidekickRankingParams
   */
  public function getRankingParams()
  {
    return $this->rankingParams;
  }
  /**
   * @param string
   */
  public function setRenderMode($renderMode)
  {
    $this->renderMode = $renderMode;
  }
  /**
   * @return string
   */
  public function getRenderMode()
  {
    return $this->renderMode;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EnterpriseTopazSidekickCardMetadata::class, 'Google_Service_CloudSearch_EnterpriseTopazSidekickCardMetadata');
