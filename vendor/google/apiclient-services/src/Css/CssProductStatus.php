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

namespace Google\Service\Css;

class CssProductStatus extends \Google\Collection
{
  protected $collection_key = 'itemLevelIssues';
  /**
   * @var string
   */
  public $creationDate;
  protected $destinationStatusesType = DestinationStatus::class;
  protected $destinationStatusesDataType = 'array';
  /**
   * @var string
   */
  public $googleExpirationDate;
  protected $itemLevelIssuesType = ItemLevelIssue::class;
  protected $itemLevelIssuesDataType = 'array';
  /**
   * @var string
   */
  public $lastUpdateDate;

  /**
   * @param string
   */
  public function setCreationDate($creationDate)
  {
    $this->creationDate = $creationDate;
  }
  /**
   * @return string
   */
  public function getCreationDate()
  {
    return $this->creationDate;
  }
  /**
   * @param DestinationStatus[]
   */
  public function setDestinationStatuses($destinationStatuses)
  {
    $this->destinationStatuses = $destinationStatuses;
  }
  /**
   * @return DestinationStatus[]
   */
  public function getDestinationStatuses()
  {
    return $this->destinationStatuses;
  }
  /**
   * @param string
   */
  public function setGoogleExpirationDate($googleExpirationDate)
  {
    $this->googleExpirationDate = $googleExpirationDate;
  }
  /**
   * @return string
   */
  public function getGoogleExpirationDate()
  {
    return $this->googleExpirationDate;
  }
  /**
   * @param ItemLevelIssue[]
   */
  public function setItemLevelIssues($itemLevelIssues)
  {
    $this->itemLevelIssues = $itemLevelIssues;
  }
  /**
   * @return ItemLevelIssue[]
   */
  public function getItemLevelIssues()
  {
    return $this->itemLevelIssues;
  }
  /**
   * @param string
   */
  public function setLastUpdateDate($lastUpdateDate)
  {
    $this->lastUpdateDate = $lastUpdateDate;
  }
  /**
   * @return string
   */
  public function getLastUpdateDate()
  {
    return $this->lastUpdateDate;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CssProductStatus::class, 'Google_Service_Css_CssProductStatus');
