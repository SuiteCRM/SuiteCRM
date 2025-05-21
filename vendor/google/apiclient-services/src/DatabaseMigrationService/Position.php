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

namespace Google\Service\DatabaseMigrationService;

class Position extends \Google\Model
{
  /**
   * @var int
   */
  public $column;
  /**
   * @var int
   */
  public $length;
  /**
   * @var int
   */
  public $line;
  /**
   * @var int
   */
  public $offset;

  /**
   * @param int
   */
  public function setColumn($column)
  {
    $this->column = $column;
  }
  /**
   * @return int
   */
  public function getColumn()
  {
    return $this->column;
  }
  /**
   * @param int
   */
  public function setLength($length)
  {
    $this->length = $length;
  }
  /**
   * @return int
   */
  public function getLength()
  {
    return $this->length;
  }
  /**
   * @param int
   */
  public function setLine($line)
  {
    $this->line = $line;
  }
  /**
   * @return int
   */
  public function getLine()
  {
    return $this->line;
  }
  /**
   * @param int
   */
  public function setOffset($offset)
  {
    $this->offset = $offset;
  }
  /**
   * @return int
   */
  public function getOffset()
  {
    return $this->offset;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Position::class, 'Google_Service_DatabaseMigrationService_Position');
