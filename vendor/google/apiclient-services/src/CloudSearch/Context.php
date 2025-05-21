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

class Context extends \Google\Collection
{
  protected $collection_key = 'type';
  /**
   * @var string[]
   */
  public $app;
  /**
   * @var int[]
   */
  public $dayOfWeek;
  /**
   * @var string
   */
  public $endDateSec;
  /**
   * @var string
   */
  public $endDayOffsetSec;
  /**
   * @var string[]
   */
  public $locale;
  /**
   * @var string[]
   */
  public $location;
  /**
   * @var string[]
   */
  public $query;
  /**
   * @var string
   */
  public $startDateSec;
  /**
   * @var string
   */
  public $startDayOffsetSec;
  /**
   * @var string[]
   */
  public $surface;
  /**
   * @var string[]
   */
  public $type;

  /**
   * @param string[]
   */
  public function setApp($app)
  {
    $this->app = $app;
  }
  /**
   * @return string[]
   */
  public function getApp()
  {
    return $this->app;
  }
  /**
   * @param int[]
   */
  public function setDayOfWeek($dayOfWeek)
  {
    $this->dayOfWeek = $dayOfWeek;
  }
  /**
   * @return int[]
   */
  public function getDayOfWeek()
  {
    return $this->dayOfWeek;
  }
  /**
   * @param string
   */
  public function setEndDateSec($endDateSec)
  {
    $this->endDateSec = $endDateSec;
  }
  /**
   * @return string
   */
  public function getEndDateSec()
  {
    return $this->endDateSec;
  }
  /**
   * @param string
   */
  public function setEndDayOffsetSec($endDayOffsetSec)
  {
    $this->endDayOffsetSec = $endDayOffsetSec;
  }
  /**
   * @return string
   */
  public function getEndDayOffsetSec()
  {
    return $this->endDayOffsetSec;
  }
  /**
   * @param string[]
   */
  public function setLocale($locale)
  {
    $this->locale = $locale;
  }
  /**
   * @return string[]
   */
  public function getLocale()
  {
    return $this->locale;
  }
  /**
   * @param string[]
   */
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string[]
   */
  public function getLocation()
  {
    return $this->location;
  }
  /**
   * @param string[]
   */
  public function setQuery($query)
  {
    $this->query = $query;
  }
  /**
   * @return string[]
   */
  public function getQuery()
  {
    return $this->query;
  }
  /**
   * @param string
   */
  public function setStartDateSec($startDateSec)
  {
    $this->startDateSec = $startDateSec;
  }
  /**
   * @return string
   */
  public function getStartDateSec()
  {
    return $this->startDateSec;
  }
  /**
   * @param string
   */
  public function setStartDayOffsetSec($startDayOffsetSec)
  {
    $this->startDayOffsetSec = $startDayOffsetSec;
  }
  /**
   * @return string
   */
  public function getStartDayOffsetSec()
  {
    return $this->startDayOffsetSec;
  }
  /**
   * @param string[]
   */
  public function setSurface($surface)
  {
    $this->surface = $surface;
  }
  /**
   * @return string[]
   */
  public function getSurface()
  {
    return $this->surface;
  }
  /**
   * @param string[]
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string[]
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Context::class, 'Google_Service_CloudSearch_Context');
