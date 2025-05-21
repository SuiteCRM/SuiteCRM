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

namespace Google\Service\Walletobjects;

class EventDateTime extends \Google\Model
{
  protected $customDoorsOpenLabelType = LocalizedString::class;
  protected $customDoorsOpenLabelDataType = '';
  /**
   * @var string
   */
  public $doorsOpen;
  /**
   * @var string
   */
  public $doorsOpenLabel;
  /**
   * @var string
   */
  public $end;
  /**
   * @var string
   */
  public $kind;
  /**
   * @var string
   */
  public $start;

  /**
   * @param LocalizedString
   */
  public function setCustomDoorsOpenLabel(LocalizedString $customDoorsOpenLabel)
  {
    $this->customDoorsOpenLabel = $customDoorsOpenLabel;
  }
  /**
   * @return LocalizedString
   */
  public function getCustomDoorsOpenLabel()
  {
    return $this->customDoorsOpenLabel;
  }
  /**
   * @param string
   */
  public function setDoorsOpen($doorsOpen)
  {
    $this->doorsOpen = $doorsOpen;
  }
  /**
   * @return string
   */
  public function getDoorsOpen()
  {
    return $this->doorsOpen;
  }
  /**
   * @param string
   */
  public function setDoorsOpenLabel($doorsOpenLabel)
  {
    $this->doorsOpenLabel = $doorsOpenLabel;
  }
  /**
   * @return string
   */
  public function getDoorsOpenLabel()
  {
    return $this->doorsOpenLabel;
  }
  /**
   * @param string
   */
  public function setEnd($end)
  {
    $this->end = $end;
  }
  /**
   * @return string
   */
  public function getEnd()
  {
    return $this->end;
  }
  /**
   * @param string
   */
  public function setKind($kind)
  {
    $this->kind = $kind;
  }
  /**
   * @return string
   */
  public function getKind()
  {
    return $this->kind;
  }
  /**
   * @param string
   */
  public function setStart($start)
  {
    $this->start = $start;
  }
  /**
   * @return string
   */
  public function getStart()
  {
    return $this->start;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EventDateTime::class, 'Google_Service_Walletobjects_EventDateTime');
