pub struct Navigator {
    offset: usize,
    selected: usize,
    pub max_rows: usize,
    len: usize,
}

impl Navigator {
    pub fn new(max_rows: usize) -> Self {
        Self {
            offset: 0,
            selected: 0,
            max_rows,
            len: 0,
        }
    }

    pub fn set_len(&mut self, len: usize) {
        self.len = len;
        self.offset = 0;
        self.selected = 0;
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn selected(&self) -> usize {
        self.selected
    }

    pub fn move_selection(&mut self, direction: i32) {
        if self.len == 0 {
            return;
        }

        if self.len <= self.max_rows {
            self.selected = (self.selected as isize + direction as isize)
                .rem_euclid(self.len as isize) as usize;
        } else {
            match direction {
                d if d > 0 && self.selected < self.max_rows - 1 => self.selected += 1,
                d if d > 0 => self.offset = (self.offset + 1) % self.len,
                d if d < 0 && self.selected > 0 => self.selected -= 1,
                d if d < 0 => self.offset = (self.offset + self.len - 1) % self.len,
                _ => {}
            }
        }
    }

    pub fn jump_to_start(&mut self) {
        if self.len == 0 {
            return;
        }
        self.offset = 0;
        self.selected = 0;
    }

    pub fn jump_to_end(&mut self) {
        if self.len == 0 {
            return;
        }
        if self.len <= self.max_rows {
            self.selected = self.len - 1;
        } else {
            self.offset = self.len - self.max_rows;
            self.selected = self.max_rows - 1;
        }
    }

    pub fn current_index(&self) -> usize {
        (self.offset + self.selected) % self.len
    }
}
