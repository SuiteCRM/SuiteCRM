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

namespace Google\Service\AuthorizedBuyersMarketplace;

class AuctionPackage extends \Google\Collection
{
  protected $collection_key = 'subscribedMediaPlanners';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $creator;
  /**
   * @var string
   */
  public $dealOwnerSeatId;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string[]
   */
  public $eligibleSeatIds;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string[]
   */
  public $subscribedBuyers;
  /**
   * @var string[]
   */
  public $subscribedClients;
  protected $subscribedMediaPlannersType = MediaPlanner::class;
  protected $subscribedMediaPlannersDataType = 'array';
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
   * @param string
   */
  public function setCreator($creator)
  {
    $this->creator = $creator;
  }
  /**
   * @return string
   */
  public function getCreator()
  {
    return $this->creator;
  }
  /**
   * @param string
   */
  public function setDealOwnerSeatId($dealOwnerSeatId)
  {
    $this->dealOwnerSeatId = $dealOwnerSeatId;
  }
  /**
   * @return string
   */
  public function getDealOwnerSeatId()
  {
    return $this->dealOwnerSeatId;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string[]
   */
  public function setEligibleSeatIds($eligibleSeatIds)
  {
    $this->eligibleSeatIds = $eligibleSeatIds;
  }
  /**
   * @return string[]
   */
  public function getEligibleSeatIds()
  {
    return $this->eligibleSeatIds;
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
  public function setSubscribedBuyers($subscribedBuyers)
  {
    $this->subscribedBuyers = $subscribedBuyers;
  }
  /**
   * @return string[]
   */
  public function getSubscribedBuyers()
  {
    return $this->subscribedBuyers;
  }
  /**
   * @param string[]
   */
  public function setSubscribedClients($subscribedClients)
  {
    $this->subscribedClients = $subscribedClients;
  }
  /**
   * @return string[]
   */
  public function getSubscribedClients()
  {
    return $this->subscribedClients;
  }
  /**
   * @param MediaPlanner[]
   */
  public function setSubscribedMediaPlanners($subscribedMediaPlanners)
  {
    $this->subscribedMediaPlanners = $subscribedMediaPlanners;
  }
  /**
   * @return MediaPlanner[]
   */
  public function getSubscribedMediaPlanners()
  {
    return $this->subscribedMediaPlanners;
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
class_alias(AuctionPackage::class, 'Google_Service_AuthorizedBuyersMarketplace_AuctionPackage');
