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

namespace Google\Service\AnalyticsData;

class AudienceExport extends \Google\Collection
{
  protected $collection_key = 'dimensions';
  /**
   * @var string
   */
  public $audience;
  /**
   * @var string
   */
  public $audienceDisplayName;
  /**
   * @var string
   */
  public $beginCreatingTime;
  /**
   * @var int
   */
  public $creationQuotaTokensCharged;
  protected $dimensionsType = V1betaAudienceDimension::class;
  protected $dimensionsDataType = 'array';
  /**
   * @var string
   */
  public $errorMessage;
  /**
   * @var string
   */
  public $name;
  public $percentageCompleted;
  /**
   * @var int
   */
  public $rowCount;
  /**
   * @var string
   */
  public $state;

  /**
   * @param string
   */
  public function setAudience($audience)
  {
    $this->audience = $audience;
  }
  /**
   * @return string
   */
  public function getAudience()
  {
    return $this->audience;
  }
  /**
   * @param string
   */
  public function setAudienceDisplayName($audienceDisplayName)
  {
    $this->audienceDisplayName = $audienceDisplayName;
  }
  /**
   * @return string
   */
  public function getAudienceDisplayName()
  {
    return $this->audienceDisplayName;
  }
  /**
   * @param string
   */
  public function setBeginCreatingTime($beginCreatingTime)
  {
    $this->beginCreatingTime = $beginCreatingTime;
  }
  /**
   * @return string
   */
  public function getBeginCreatingTime()
  {
    return $this->beginCreatingTime;
  }
  /**
   * @param int
   */
  public function setCreationQuotaTokensCharged($creationQuotaTokensCharged)
  {
    $this->creationQuotaTokensCharged = $creationQuotaTokensCharged;
  }
  /**
   * @return int
   */
  public function getCreationQuotaTokensCharged()
  {
    return $this->creationQuotaTokensCharged;
  }
  /**
   * @param V1betaAudienceDimension[]
   */
  public function setDimensions($dimensions)
  {
    $this->dimensions = $dimensions;
  }
  /**
   * @return V1betaAudienceDimension[]
   */
  public function getDimensions()
  {
    return $this->dimensions;
  }
  /**
   * @param string
   */
  public function setErrorMessage($errorMessage)
  {
    $this->errorMessage = $errorMessage;
  }
  /**
   * @return string
   */
  public function getErrorMessage()
  {
    return $this->errorMessage;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  public function setPercentageCompleted($percentageCompleted)
  {
    $this->percentageCompleted = $percentageCompleted;
  }
  public function getPercentageCompleted()
  {
    return $this->percentageCompleted;
  }
  /**
   * @param int
   */
  public function setRowCount($rowCount)
  {
    $this->rowCount = $rowCount;
  }
  /**
   * @return int
   */
  public function getRowCount()
  {
    return $this->rowCount;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AudienceExport::class, 'Google_Service_AnalyticsData_AudienceExport');
