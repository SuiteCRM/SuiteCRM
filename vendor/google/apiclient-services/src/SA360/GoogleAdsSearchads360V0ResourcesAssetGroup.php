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

namespace Google\Service\SA360;

class GoogleAdsSearchads360V0ResourcesAssetGroup extends \Google\Collection
{
  protected $collection_key = 'finalUrls';
  /**
   * @var string
   */
  public $adStrength;
  /**
   * @var string
   */
  public $campaign;
  /**
   * @var string[]
   */
  public $finalMobileUrls;
  /**
   * @var string[]
   */
  public $finalUrls;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $path1;
  /**
   * @var string
   */
  public $path2;
  /**
   * @var string
   */
  public $resourceName;
  /**
   * @var string
   */
  public $status;

  /**
   * @param string
   */
  public function setAdStrength($adStrength)
  {
    $this->adStrength = $adStrength;
  }
  /**
   * @return string
   */
  public function getAdStrength()
  {
    return $this->adStrength;
  }
  /**
   * @param string
   */
  public function setCampaign($campaign)
  {
    $this->campaign = $campaign;
  }
  /**
   * @return string
   */
  public function getCampaign()
  {
    return $this->campaign;
  }
  /**
   * @param string[]
   */
  public function setFinalMobileUrls($finalMobileUrls)
  {
    $this->finalMobileUrls = $finalMobileUrls;
  }
  /**
   * @return string[]
   */
  public function getFinalMobileUrls()
  {
    return $this->finalMobileUrls;
  }
  /**
   * @param string[]
   */
  public function setFinalUrls($finalUrls)
  {
    $this->finalUrls = $finalUrls;
  }
  /**
   * @return string[]
   */
  public function getFinalUrls()
  {
    return $this->finalUrls;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
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
   * @param string
   */
  public function setPath1($path1)
  {
    $this->path1 = $path1;
  }
  /**
   * @return string
   */
  public function getPath1()
  {
    return $this->path1;
  }
  /**
   * @param string
   */
  public function setPath2($path2)
  {
    $this->path2 = $path2;
  }
  /**
   * @return string
   */
  public function getPath2()
  {
    return $this->path2;
  }
  /**
   * @param string
   */
  public function setResourceName($resourceName)
  {
    $this->resourceName = $resourceName;
  }
  /**
   * @return string
   */
  public function getResourceName()
  {
    return $this->resourceName;
  }
  /**
   * @param string
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return string
   */
  public function getStatus()
  {
    return $this->status;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesAssetGroup::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesAssetGroup');
