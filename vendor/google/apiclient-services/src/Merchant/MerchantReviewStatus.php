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

namespace Google\Service\Merchant;

class MerchantReviewStatus extends \Google\Collection
{
  protected $collection_key = 'itemLevelIssues';
  /**
   * @var string
   */
  public $createTime;
  protected $destinationStatusesType = MerchantReviewDestinationStatus::class;
  protected $destinationStatusesDataType = 'array';
  protected $itemLevelIssuesType = MerchantReviewItemLevelIssue::class;
  protected $itemLevelIssuesDataType = 'array';
  /**
   * @var string
   */
  public $lastUpdateTime;

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
   * @param MerchantReviewDestinationStatus[]
   */
  public function setDestinationStatuses($destinationStatuses)
  {
    $this->destinationStatuses = $destinationStatuses;
  }
  /**
   * @return MerchantReviewDestinationStatus[]
   */
  public function getDestinationStatuses()
  {
    return $this->destinationStatuses;
  }
  /**
   * @param MerchantReviewItemLevelIssue[]
   */
  public function setItemLevelIssues($itemLevelIssues)
  {
    $this->itemLevelIssues = $itemLevelIssues;
  }
  /**
   * @return MerchantReviewItemLevelIssue[]
   */
  public function getItemLevelIssues()
  {
    return $this->itemLevelIssues;
  }
  /**
   * @param string
   */
  public function setLastUpdateTime($lastUpdateTime)
  {
    $this->lastUpdateTime = $lastUpdateTime;
  }
  /**
   * @return string
   */
  public function getLastUpdateTime()
  {
    return $this->lastUpdateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(MerchantReviewStatus::class, 'Google_Service_Merchant_MerchantReviewStatus');
