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

namespace Google\Service\Dfareporting;

class TvCampaignTimepoint extends \Google\Model
{
  /**
   * @var string
   */
  public $dateWindow;
  public $spend;
  /**
   * @var string
   */
  public $startDate;

  /**
   * @param string
   */
  public function setDateWindow($dateWindow)
  {
    $this->dateWindow = $dateWindow;
  }
  /**
   * @return string
   */
  public function getDateWindow()
  {
    return $this->dateWindow;
  }
  public function setSpend($spend)
  {
    $this->spend = $spend;
  }
  public function getSpend()
  {
    return $this->spend;
  }
  /**
   * @param string
   */
  public function setStartDate($startDate)
  {
    $this->startDate = $startDate;
  }
  /**
   * @return string
   */
  public function getStartDate()
  {
    return $this->startDate;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TvCampaignTimepoint::class, 'Google_Service_Dfareporting_TvCampaignTimepoint');
