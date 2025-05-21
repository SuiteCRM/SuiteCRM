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

namespace Google\Service\CloudControlsPartnerService;

class Partner extends \Google\Collection
{
  protected $collection_key = 'skus';
  /**
   * @var string
   */
  public $createTime;
  protected $ekmSolutionsType = EkmMetadata::class;
  protected $ekmSolutionsDataType = 'array';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string[]
   */
  public $operatedCloudRegions;
  /**
   * @var string
   */
  public $partnerProjectId;
  protected $skusType = Sku::class;
  protected $skusDataType = 'array';
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param EkmMetadata[]
   */
  public function setEkmSolutions($ekmSolutions)
  {
    $this->ekmSolutions = $ekmSolutions;
  }
  /**
   * @return EkmMetadata[]
   */
  public function getEkmSolutions()
  {
    return $this->ekmSolutions;
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
  /**
   * @param string[]
   */
  public function setOperatedCloudRegions($operatedCloudRegions)
  {
    $this->operatedCloudRegions = $operatedCloudRegions;
  }
  /**
   * @return string[]
   */
  public function getOperatedCloudRegions()
  {
    return $this->operatedCloudRegions;
  }
  /**
   * @param string
   */
  public function setPartnerProjectId($partnerProjectId)
  {
    $this->partnerProjectId = $partnerProjectId;
  }
  /**
   * @return string
   */
  public function getPartnerProjectId()
  {
    return $this->partnerProjectId;
  }
  /**
   * @param Sku[]
   */
  public function setSkus($skus)
  {
    $this->skus = $skus;
  }
  /**
   * @return Sku[]
   */
  public function getSkus()
  {
    return $this->skus;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Partner::class, 'Google_Service_CloudControlsPartnerService_Partner');
