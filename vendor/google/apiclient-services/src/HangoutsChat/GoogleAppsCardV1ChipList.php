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

namespace Google\Service\HangoutsChat;

class GoogleAppsCardV1ChipList extends \Google\Collection
{
  protected $collection_key = 'chips';
  protected $chipsType = GoogleAppsCardV1Chip::class;
  protected $chipsDataType = 'array';
  /**
   * @var string
   */
  public $layout;

  /**
   * @param GoogleAppsCardV1Chip[]
   */
  public function setChips($chips)
  {
    $this->chips = $chips;
  }
  /**
   * @return GoogleAppsCardV1Chip[]
   */
  public function getChips()
  {
    return $this->chips;
  }
  /**
   * @param string
   */
  public function setLayout($layout)
  {
    $this->layout = $layout;
  }
  /**
   * @return string
   */
  public function getLayout()
  {
    return $this->layout;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAppsCardV1ChipList::class, 'Google_Service_HangoutsChat_GoogleAppsCardV1ChipList');
